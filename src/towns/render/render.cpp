/* LICENSE>>
Copyright 2020 Soji Yamakawa (CaptainYS, http://www.ysflight.com)

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

<< LICENSE */
#include <algorithm>
#include <cstring>
#include <utility>
#include "cpputil.h"
#include "render.h"


TownsRender::TownsRender()
{
	wid=0;
	hei=0;
}
void TownsRender::Create(int wid,int hei)
{
	this->wid=wid;
	this->hei=hei;
	rgba.resize(4*wid*hei);
}

namespace {

struct layer {
	int page;
	uint32_t vramBase;
	uint32_t startAddress;
	uint32_t lineOffset;
	uint32_t fieldMask;
	uint32_t vramLoMask;
	uint32_t vramHiMask;
	uint32_t fetchRatio;
	uint32_t hZoom;
	uint32_t vZoom;

	uint32_t haj;
	uint32_t hds, hde;
	uint32_t vds, vde;

	uint32_t shifterMask;
	uint32_t shifterCount;

	bool interlace;
	uint32_t fieldOffset;
};

struct screen {
	uint32_t width;
	uint32_t height;
	uint32_t hds;
	uint32_t vds2x;
	bool interlace;

	layer layers[2];
};


screen getScreen(const TownsCRTC &crtc) {
	auto &state = crtc.state;
	auto clk = crtc.CLKSELtoFreq[state.clksel()];

	auto hds = std::min(state.hds(false), state.hds(true));
	auto hde = std::max(state.hde(false), state.hde(true));
	if (hde < hds) {
		hde = hds + 1;
	}

	auto vlines2x = state.vst() + 1;
	auto vds2x = std::min(state.vds(false), state.vds(true));
	auto vde2x = std::max(state.vde(false), state.vde(true));
	if (vde2x < vds2x) {
		vde2x = vds2x + 2;
	}

	screen r;
	if (vlines2x % 2 == 1) { // ???
		r.interlace = true;
		r.hds = hds;
		r.vds2x = vds2x;
		r.width = hde - hds;
		r.height = vde2x - vds2x;
	} else {
		r.interlace = false;
		r.hds = hds;
		r.vds2x = vds2x;
		r.width = hde - hds;
		r.height = (vde2x - vds2x) / 2;

		// 498以上では画面が表示されない(?)
		r.height = std::min(r.height, 497u);
	}

	for (int i = 0; i < 2; ++i) {
		layer &l = r.layers[i];
		bool one = i;

		l.page = static_cast<int>(one);
		l.vramBase = one ? 0x40000 : 0;
		l.startAddress = state.fa(one) * 4;
		l.fieldOffset = state.fo(one) * 4;
		l.lineOffset = state.lo(one) * 4;
		l.vramLoMask = state.cen(one) ? 0x3ffff : 0x003ff;
		l.vramHiMask = state.cen(one) ? 0x00000 : 0x3fc00;
		l.hZoom = state.zh(one) + 1;
		l.vZoom = state.zv(one) + 1;
		l.fetchRatio = 1 << state.cl(one);

		l.haj = state.haj(one) - r.hds;
		l.hds = state.hds(one) - r.hds;
		l.hde = state.hde(one) - r.hds;
		l.vds = 0;
		l.vde = r.height;

		if (state.s_pmode()) {
			switch (state.s_cl(one)) {
			case 1:
				l.shifterCount = 4;
				l.shifterMask = 0x0f;
				break;
			case 3:
				l.shifterCount = 16;
				l.shifterMask = 0xffff;
				break;
			default:
				l.shifterCount = 0;
				l.shifterMask = 0;
				break;
			}
		}
		else {
			switch (state.s_cl(one)) {
			case 2:
				l.shifterCount = 8;
				l.shifterMask = 0xff;
				break;
			case 3:
				l.shifterCount = 16;
				l.shifterMask = 0xffff;
				break;
			default:
				l.shifterCount = 0;
				l.shifterMask = 0;
				break;
			}
		}
	}

	return r;
}

void render1(std::vector<uint8_t> &rgba, const screen &sc, const TownsCRTC::AnalogPalette &palette, const std::vector<uint8_t> &VRAM) {
}

void render2(std::vector<uint8_t> &rgba, const screen &sc, int page, const TownsCRTC::AnalogPalette &palette, const std::vector<uint8_t> &VRAM) {
	auto &layer = sc.layers[page];
	uint32_t addr = layer.startAddress;
	int nextLine = layer.vZoom;

	// VDS, VDEをまだ考えていない
	for (uint32_t y = 0; y < sc.height; ++y) {
		uint32_t data;
		uint16_t val;
		int nextFetch = 1;
		int nextShift = 1;

		// 剰余について考える
		// HDS < HAJ のときは壊れた内容が描画されるはず
		int offset = ((layer.hds - layer.haj) * 4) / (layer.fetchRatio * layer.hZoom);

		for (uint32_t x = layer.hds; x < layer.hde; ++x) {
			if (--nextFetch == 0) {
				auto d = (addr & layer.vramHiMask) | ((addr + offset) & layer.vramLoMask);
				data = cpputil::GetDword(VRAM.data() + layer.vramBase + d);
				nextFetch = layer.fetchRatio * layer.hZoom;
				offset += 4;
			}

			if (--nextShift == 0) {
				val = data & layer.shifterMask;
				data >>= layer.shifterCount;
				nextShift = layer.hZoom;
			}

			auto dst = rgba.begin() + ((y * sc.width) + x) * 4;
			switch (layer.shifterCount) {
			case 4:
				if (val) {
					dst[0] = palette.plt16[layer.page][val][0];
					dst[1] = palette.plt16[layer.page][val][1];
					dst[2] = palette.plt16[layer.page][val][2];
					dst[3] = 255;
				}
				break;
			case 8:
				if (val) {
					dst[0] = palette.plt256[val][0];
					dst[1] = palette.plt256[val][1];
					dst[2] = palette.plt256[val][2];
					dst[3] = 255;
				}
				break;
			case 16:
				if ((val & 0x8000) == 0) {
					dst[0] = ((val & 0x03e0) >>  5) << 3;
					dst[1] = ((val & 0x7c00) >> 10) << 3;
					dst[2] = ((val & 0x001f)      ) << 3;
					dst[3] = 255;
				}
				break;
			}
		}
		if (--nextLine == 0) {
			addr += layer.lineOffset;
			nextLine = layer.vZoom;
		}
	}
}

}

void TownsRender::BuildImage(const TownsCRTC &crtc,const TownsPhysicalMemory &physMem)
{
	screen sc = getScreen(crtc);
	SetResolution(sc.width, sc.height);

	std::memset(rgba.data(), 0, rgba.size());

	if(crtc.InSinglePageMode())
	{
		if (crtc.state.ShowPage(0))
		{
			TownsCRTC::Layer layer;
			crtc.MakePageLayerInfo(layer, 0);
			Render<VRAM1Trans>(0, layer, crtc.state.palette, crtc.chaseHQPalette, physMem.state.VRAM, false);
		}
	}
	else
	{
		auto priorityPage = crtc.GetPriorityPage();
		if (crtc.state.ShowPage(1 - priorityPage))
		{
			render2(rgba, sc, 1 - priorityPage, crtc.state.palette, physMem.state.VRAM);
		}
		if (crtc.state.ShowPage(priorityPage))
		{
			render2(rgba, sc, priorityPage, crtc.state.palette, physMem.state.VRAM);
		}
	}
}

void TownsRender::SetResolution(int wid,int hei)
{
	if(wid!=this->wid || hei!=this->hei)
	{
		Create(wid,hei);
	}
}

TownsRender::Image TownsRender::GetImage(void) const
{
	Image img;
	img.wid=wid;
	img.hei=hei;
	img.rgba=this->rgba.data();
	return img;
}

template <class OFFSETTRANS>
void TownsRender::Render(
    unsigned int page,
    const TownsCRTC::Layer &layer,
    const TownsCRTC::AnalogPalette &palette,
    const TownsCRTC::ChaseHQPalette &chaseHQPalette,
    const std::vector <unsigned char> &VRAM,
    bool transparent)
{
	switch(layer.bitsPerPixel)
	{
	case 4:
		Render4Bit<OFFSETTRANS>(layer,palette.plt16[page&1],chaseHQPalette,VRAM,transparent);
		break;
	case 8:
		Render8Bit<OFFSETTRANS>(layer,palette.plt256,VRAM,transparent);
		break;
	case 16:
		Render16Bit<OFFSETTRANS>(layer,VRAM,transparent);
		break;
	}
}
template <class OFFSETTRANS>
void TownsRender::Render4Bit(
    const TownsCRTC::Layer &layer,
    const Vec3ub palette[16],
    const TownsCRTC::ChaseHQPalette &chaseHQPalette,
    const std::vector <unsigned char> &VRAM,
    bool transparent)
{
	const unsigned int VRAMAddr=layer.VRAMAddr;

/*	if(layer.zoom==Vec2i::Make(1,1))
	{
		for(int y=0; y<layer.sizeOnMonitor.y(); ++y)
		{
			const unsigned char *src=VRAM.data()+VRAMAddr+((layer.VRAMOffset+layer.bytesPerLine*y)&layer.VScrollMask);
			unsigned char *dst=rgba.data()+4*y*this->wid;
			for(int x=0; x<layer.sizeOnMonitor.x(); x+=2)
			{
				unsigned char vrambyte=*src;
				unsigned char pix=(vrambyte&0x0f);
				if(0!=pix ||true!=transparent)
				{
					dst[0]=palette[pix][0];
					dst[1]=palette[pix][1];
					dst[2]=palette[pix][2];
					dst[3]=255;
				}
				pix=(vrambyte&0xf0)>>4;
				if(0!=pix ||true!=transparent)
				{
					dst[4]=palette[pix][0];
					dst[5]=palette[pix][1];
					dst[6]=palette[pix][2];
					dst[7]=255;
				}
				++src;
				dst+=8;
			}
		}
	}
	else */

	if(47!=chaseHQPalette.lastPaletteUpdateCount) // ChaseHQ updates palette 47 times between VSYNC
	{
		auto ZV0=layer.zoom2x.y()/2;
		auto ZV=ZV0;
		const int ZH[2]={layer.zoom2x.x()/2,(layer.zoom2x.x()+1)/2};  // For x.5 times zoom rate.
		int bytesPerLineTimesVRAMy=layer.VRAMOffset;
		auto VRAMTop=VRAM.data()+VRAMAddr+layer.VRAMHSkipBytes;

		// yStep should be 1 if transparent.
		// If transparnet==true, there is a possibility that memcpy overwrites background pixels.
		unsigned int yStep=(true!=transparent ? ZV : 1);
		auto bottomY=this->hei-yStep;
		for(int y=0; y<layer.sizeOnMonitor.y() && y+layer.originOnMonitor.y()<=bottomY; y+=yStep)
		{
			const int Y=y+layer.originOnMonitor.y();
			const int X=  layer.originOnMonitor.x();
			unsigned int VRAMAddr=(bytesPerLineTimesVRAMy&layer.VScrollMask);
			OFFSETTRANS::Trans(VRAMAddr);
			const unsigned char *src=VRAMTop+VRAMAddr;
			unsigned char *dstLine=rgba.data()+4*(Y*this->wid+X);
			auto dst=dstLine;
			for(int x=0; x<layer.sizeOnMonitor.x() && x+layer.originOnMonitor.x()<this->wid; x+=(ZH[0]+ZH[1]))
			{
				unsigned char vrambyte=*src;
				unsigned char pix=(vrambyte&0x0f);
				for(int i=0; i<ZH[0]; ++i)
				{
					if(0!=pix ||true!=transparent)
					{
						dst[0]=palette[pix][0];
						dst[1]=palette[pix][1];
						dst[2]=palette[pix][2];
						dst[3]=255;
					}
					dst+=4;
				}
				pix=(vrambyte&0xf0)>>4;
				for(int i=0; i<ZH[1]; ++i)
				{
					if(0!=pix ||true!=transparent)
					{
						dst[0]=palette[pix][0];
						dst[1]=palette[pix][1];
						dst[2]=palette[pix][2];
						dst[3]=255;
					}
					dst+=4;
				}
				++src;
			}

			if(1<yStep)
			{
				auto copyPtr=dstLine+(4*this->wid);
				for(unsigned int zv=1; zv<yStep; ++zv)
				{
					std::memcpy(copyPtr,dstLine,dst-dstLine);
					copyPtr+=(4*this->wid);
				}
				bytesPerLineTimesVRAMy+=layer.bytesPerLine;
			}
			else
			{
				--ZV;
				if(0==ZV)
				{
					ZV=ZV0;
					bytesPerLineTimesVRAMy+=layer.bytesPerLine;
				}
			}
		}
	}
	else // For ChaseHQ special: If 16-color mode, and palette changed more than 40 times in one frame.
	{
		// Roughly 98:46:95
		// std::cout << "ChaseHQ Special" << " " << chaseHQPalette.lastPaletteUpdateCount << std::endl;

		Vec3ub paletteUpdate[16];
		for(int i=0; i<16; ++i)
		{
			paletteUpdate[i]=palette[i];
		}

		for(int i=0; i<16; ++i) // Sky
		{
			unsigned int code=chaseHQPalette.paletteLog[i<<2];
			paletteUpdate[code&0x0F][0]=chaseHQPalette.paletteLog[(i<<2)+2];   // BRG->RGB
			paletteUpdate[code&0x0F][1]=chaseHQPalette.paletteLog[(i<<2)+3];   // BRG->RGB
			paletteUpdate[code&0x0F][2]=chaseHQPalette.paletteLog[(i<<2)+1];   // BRG->RGB
		}

		auto ZV=layer.zoom2x.y()/2;
		const int ZH[2]={layer.zoom2x.x()/2,(layer.zoom2x.x()+1)/2};  // For x.5 times zoom rate.
		int bytesPerLineTimesVRAMy=layer.VRAMOffset;
		auto VRAMTop=VRAM.data()+VRAMAddr+layer.VRAMHSkipBytes;

		auto bottomY=this->hei-ZV;
		for(int y=0; y<layer.sizeOnMonitor.y() && y+layer.originOnMonitor.y()<=bottomY; y+=ZV)
		{
			const int Y=y+layer.originOnMonitor.y();
			const int X=  layer.originOnMonitor.x();

			if(196==y)
			{
				for(int i=16; i<32; ++i) // Buildings
				{
					unsigned int code=chaseHQPalette.paletteLog[i<<2];
					paletteUpdate[code&0x0F][0]=chaseHQPalette.paletteLog[(i<<2)+2];   // BRG->RGB
					paletteUpdate[code&0x0F][1]=chaseHQPalette.paletteLog[(i<<2)+3];   // BRG->RGB
					paletteUpdate[code&0x0F][2]=chaseHQPalette.paletteLog[(i<<2)+1];   // BRG->RGB
				}
			}
			if(288==y)
			{
				for(int i=32; i<47; ++i) // Road
				{
					unsigned int code=chaseHQPalette.paletteLog[i<<2];
					paletteUpdate[code&0x0F][0]=chaseHQPalette.paletteLog[(i<<2)+2];   // BRG->RGB
					paletteUpdate[code&0x0F][1]=chaseHQPalette.paletteLog[(i<<2)+3];   // BRG->RGB
					paletteUpdate[code&0x0F][2]=chaseHQPalette.paletteLog[(i<<2)+1];   // BRG->RGB
				}
			}

			unsigned int VRAMAddr=(bytesPerLineTimesVRAMy&layer.VScrollMask);
			OFFSETTRANS::Trans(VRAMAddr);
			const unsigned char *src=VRAMTop+VRAMAddr;
			unsigned char *dstLine=rgba.data()+4*(Y*this->wid+X);
			auto dst=dstLine;
			for(int x=0; x<layer.sizeOnMonitor.x() && x+layer.originOnMonitor.x()<this->wid; x+=(ZH[0]+ZH[1]))
			{
				unsigned char vrambyte=*src;
				unsigned char pix=(vrambyte&0x0f);
				for(int i=0; i<ZH[0]; ++i)
				{
					if(0!=pix ||true!=transparent)
					{
						dst[0]=paletteUpdate[pix][0];
						dst[1]=paletteUpdate[pix][1];
						dst[2]=paletteUpdate[pix][2];
						dst[3]=255;
					}
					dst+=4;
				}
				pix=(vrambyte&0xf0)>>4;
				for(int i=0; i<ZH[1]; ++i)
				{
					if(0!=pix ||true!=transparent)
					{
						dst[0]=paletteUpdate[pix][0];
						dst[1]=paletteUpdate[pix][1];
						dst[2]=paletteUpdate[pix][2];
						dst[3]=255;
					}
					dst+=4;
				}
				++src;
			}

			auto copyPtr=dstLine+(4*this->wid);
			for(unsigned int zv=1; zv<ZV; ++zv)
			{
				std::memcpy(copyPtr,dstLine,dst-dstLine);
				copyPtr+=(4*this->wid);
			}

			bytesPerLineTimesVRAMy+=layer.bytesPerLine;
		}
	}
}
template <class OFFSETTRANS>
void TownsRender::Render8Bit(const TownsCRTC::Layer &layer,const Vec3ub palette[256],const std::vector <unsigned char> &VRAM,bool transparent)
{
	unsigned int VRAMBase=layer.VRAMAddr+layer.VRAMHSkipBytes;
	unsigned int VRAMOffsetVertical=layer.VRAMOffset&~layer.HScrollMask;
	unsigned int VRAMOffsetHorizontal=layer.VRAMOffset&layer.HScrollMask;
	const unsigned int VRAMHScrollMask=layer.HScrollMask;
	const unsigned int VRAMVScrollMask=layer.VScrollMask;
	unsigned int lineVRAMOffset=0;
	const int ZHsrc[2]={layer.zoom2x.x()/2,(layer.zoom2x.x()+1)/2};  // For x.5 times zoom rate.
	auto ZV=layer.zoom2x.y()/2;

	auto bottomY=this->hei-ZV;
	for(int y=0; y<layer.sizeOnMonitor.y() && y+layer.originOnMonitor.y()<=bottomY; y+=ZV)
	{
		auto X=  layer.originOnMonitor.x();
		auto Y=y+layer.originOnMonitor.y();
		unsigned char *dstLine=rgba.data()+4*(Y*this->wid+X);
		auto dst=dstLine;

		unsigned int inLineVRAMOffset=0;
		int ZHswitch=0;
		auto ZH=ZHsrc[ZHswitch];
		for(int x=0; x<layer.sizeOnMonitor.x() && x+layer.originOnMonitor.x()<this->wid && inLineVRAMOffset<layer.bytesPerLine; x++)
		{
			unsigned int VRAMAddr=lineVRAMOffset+((inLineVRAMOffset+VRAMOffsetHorizontal)&VRAMHScrollMask);
			VRAMAddr=VRAMBase+((VRAMAddr+VRAMOffsetVertical)&VRAMVScrollMask);
			OFFSETTRANS::Trans(VRAMAddr);

			unsigned char col8=VRAM[VRAMAddr];
			if(true!=transparent || 0!=col8)
			{
				dst[0]=palette[col8][0];
				dst[1]=palette[col8][1];
				dst[2]=palette[col8][2];
				dst[3]=255;
			}
			dst+=4;
			if(0==(--ZH))
			{
				ZHswitch=1-ZHswitch;
				ZH=ZHsrc[ZHswitch];
				++inLineVRAMOffset;
			}
		}

		auto copyPtr=dstLine+(4*this->wid);
		for(unsigned int zv=1; zv<ZV; ++zv)
		{
			std::memcpy(copyPtr,dstLine,dst-dstLine);
			copyPtr+=(4*this->wid);
		}

		lineVRAMOffset+=layer.bytesPerLine;
	}
}
template <class OFFSETTRANS>
void TownsRender::Render16Bit(const TownsCRTC::Layer &layer,const std::vector <unsigned char> &VRAM,bool transparent)
{
	unsigned int VRAMBase=layer.VRAMAddr+layer.VRAMHSkipBytes;
	unsigned int VRAMOffsetVertical=layer.VRAMOffset&~layer.HScrollMask;
	unsigned int VRAMOffsetHorizontal=layer.VRAMOffset&layer.HScrollMask;
	const unsigned int VRAMHScrollMask=layer.HScrollMask;
	const unsigned int VRAMVScrollMask=layer.VScrollMask;
	unsigned int lineVRAMOffset=0;
	const int ZHsrc[2]={layer.zoom2x.x()/2,(layer.zoom2x.x()+1)/2};  // For x.5 times zoom rate.
	auto ZV0=layer.zoom2x.y()/2;
	auto ZV=ZV0;

	// yStep should be 1 if transparent.
	// If transparnet==true, there is a possibility that memcpy overwrites background pixels.
	unsigned int yStep=(true!=transparent ? ZV0 : 1);
	auto bottomY=this->hei-yStep;
	for(int y=0; y<layer.sizeOnMonitor.y() && y+layer.originOnMonitor.y()<=bottomY; y+=yStep)
	{
		auto X=  layer.originOnMonitor.x();
		auto Y=y+layer.originOnMonitor.y();
		unsigned char *dstLine=rgba.data()+4*(Y*this->wid+X);
		auto dst=dstLine;

		unsigned int inLineVRAMOffset=0;
		int ZHswitch=0;
		auto ZH=ZHsrc[ZHswitch];
		for(int x=0; x<layer.sizeOnMonitor.x() && x+layer.originOnMonitor.x()<this->wid && inLineVRAMOffset<layer.bytesPerLine; x++)
		{
			unsigned int VRAMAddr=lineVRAMOffset+((inLineVRAMOffset+VRAMOffsetHorizontal)&VRAMHScrollMask);
			VRAMAddr=VRAMBase+((VRAMAddr+VRAMOffsetVertical)&VRAMVScrollMask);
			OFFSETTRANS::Trans(VRAMAddr);

			unsigned short col16=cpputil::GetWord(VRAM.data()+VRAMAddr);
			if(true!=transparent || 0==(col16&0x8000))
			{
				dst[0]=((col16&0b000001111100000)>>5);
				dst[0]=(dst[0]<<3)|((dst[0]>>2)&7);
				dst[1]=((col16&0b111110000000000)>>10);
				dst[1]=(dst[1]<<3)|((dst[1]>>2)&7);
				dst[2]=(col16&0b000000000011111);
				dst[2]=(dst[2]<<3)|((dst[2]>>2)&7);
				dst[3]=255;
			}
			dst+=4;
			if(0==(--ZH))
			{
				ZHswitch=1-ZHswitch;
				ZH=ZHsrc[ZHswitch];
				inLineVRAMOffset+=2;
			}
		}

		if(1<yStep)
		{
			auto copyPtr=dstLine+(4*this->wid);
			for(unsigned int zv=1; zv<yStep; ++zv)
			{
				std::memcpy(copyPtr,dstLine,dst-dstLine);
				copyPtr+=(4*this->wid);
			}
			lineVRAMOffset+=layer.bytesPerLine;
		}
		else
		{
			--ZV;
			if(0==ZV)
			{
				ZV=ZV0;
				lineVRAMOffset+=layer.bytesPerLine;
			}
		}
	}
}
