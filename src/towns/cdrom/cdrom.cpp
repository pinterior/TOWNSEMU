#include <iostream>
#include "cdrom.h"
#include "townsdef.h"
#include "towns.h"
#include "cpputil.h"



TownsCDROM::State::State()
{
	imgPtr=new DiscImage;
}
TownsCDROM::State::~State()
{
	delete imgPtr;
}
const DiscImage &TownsCDROM::State::GetDisc(void) const
{
	return *imgPtr;
}
DiscImage &TownsCDROM::State::GetDisc(void)
{
	return *imgPtr;
}
void TownsCDROM::State::ClearStatusQueue(void)
{
	statusQueue.clear();
}
void TownsCDROM::State::PushStatusQueue(unsigned char d0,unsigned char d1,unsigned char d2,unsigned char d3)
{
	statusQueue.push_back(d0);
	statusQueue.push_back(d1);
	statusQueue.push_back(d2);
	statusQueue.push_back(d3);
}
void TownsCDROM::State::Reset(void)
{
	// Do not clear imgFileName on reset.
	ResetMPU();
	enableSIRQ=false;
	enableDEI=false;
	discChanged=false;
}

void TownsCDROM::State::ResetMPU(void)
{
	SIRQ=false; // 4C0H bit 7
	DEI=false;  // 4C0H bit 6 (DMA End Flag)
	STSF=false; // 4C0H bit 5 (Software Transfer End)
	DTSF=false; // 4C0H bit 4 (DMA Transfer in progress)
	DRY=true;   // 4C0H bit 0 (Ready to receive command)

	cmdReceived=false;
	nParamQueue=0;
	for(int i=0; i<8; ++i)
	{
		paramQueue[i]=0;
		paramQueueCopy[i]=0;
	}
	ClearStatusQueue();
	DMATransfer=false;
	CPUTransfer=true;
}

////////////////////////////////////////////////////////////

TownsCDROM::TownsCDROM(class FMTowns *townsPtr,class TownsPIC *PICPtr,class TownsDMAC *DMACPtr)
{
	this->townsPtr=townsPtr;
	this->PICPtr=PICPtr;
	this->DMACPtr=DMACPtr;
	state.Reset();
}

/* virtual */ void TownsCDROM::PowerOn(void)
{
	state.Reset();
}
/* virtual */ void TownsCDROM::Reset(void)
{
	state.Reset();
}
/* virtual */ void TownsCDROM::IOWriteByte(unsigned int ioport,unsigned int data)
{
	switch(ioport)
	{
	case TOWNSIO_CDROM_MASTER_CTRL_STATUS://0x4C0, // [2] pp.224
		if(0!=(data&0x80)) // SMIC: Clear SIRQ
		{
			state.SIRQ=false;
		}
		if(0!=(data&0x40)) // DEIC: Clera DMA-End IRQ
		{
			state.DEI=false;
		}
		if(0!=(data&0x04))
		{
			state.ResetMPU();
		}
		state.enableSIRQ=(0!=(data&0x02));
		state.enableDEI=(0!=(data&0x01));
		break;
	case TOWNSIO_CDROM_COMMAND_STATUS://    0x4C2, // [2] pp.224
		std::cout << "CDROM Command " << cpputil::Ubtox(data) << std::endl;
		state.cmdReceived=true;
		state.cmd=data;
		break;
	case TOWNSIO_CDROM_PARAMETER_DATA://    0x4C4, // [2] pp.224
		if(8<=state.nParamQueue)
		{
			for(int i=0; i<PARAM_QUEUE_LEN-1; ++i)
			{
				state.paramQueue[i]=state.paramQueue[i+1];
			}
			state.nParamQueue=PARAM_QUEUE_LEN-1;
		}
		state.paramQueue[state.nParamQueue++]=data;
		break;
	case TOWNSIO_CDROM_TRANSFER_CTRL://     0x4C6, // [2] pp.227
		state.DMATransfer=(0!=(data&0x10));
		state.CPUTransfer=(0!=(data&0x08));
		break;
	case TOWNSIO_CDROM_SUBCODE_STATUS://    0x4CC,
		// No write
		break;
	case TOWNSIO_CDROM_SUBCODE_DATA://      0x4CD,
		// No write
		break;
	}
	if(true==state.cmdReceived && PARAM_QUEUE_LEN<=state.nParamQueue)
	{
		ExecuteCDROMCommand();
	}
}
/* virtual */ unsigned int TownsCDROM::IOReadByte(unsigned int ioport)
{
	unsigned char data=0;
	switch(ioport)
	{
	case TOWNSIO_CDROM_MASTER_CTRL_STATUS://0x4C0, // [2] pp.224
		data|=(state.SIRQ ? 0x80 : 0);
		data|=(state.DEI  ? 0x40 : 0);
		data|=(state.STSF ? 0x20 : 0);
		data|=(state.DTSF ? 0x10 : 0);
		data|=(0<state.statusQueue.size() ? 2 : 0);
		data|=(state.DRY  ?    1 : 0);
		return data;
	case TOWNSIO_CDROM_COMMAND_STATUS://    0x4C2, // [2] pp.224
		if(0<state.statusQueue.size())
		{
			data=state.statusQueue.front();
			for(int i=0; i<state.statusQueue.size()-1; ++i)
			{
				state.statusQueue[i]=state.statusQueue[i+1];
			}
			state.statusQueue.pop_back();
			return data;
		}
		break;
	case TOWNSIO_CDROM_PARAMETER_DATA://    0x4C4, // [2] pp.224
		// Data register for software transfer.  Not supported yet.
		Abort("Unsupported CDROM I/O Read 04C4H");
		break;
	case TOWNSIO_CDROM_TRANSFER_CTRL://     0x4C6, // [2] pp.227
		// Write only.
		break;
	case TOWNSIO_CDROM_SUBCODE_STATUS://    0x4CC,
		Abort("Unsupported CDROM I/O Read 04CCH");
		return 0;
	case TOWNSIO_CDROM_SUBCODE_DATA://      0x4CD,
		Abort("Unsupported CDROM I/O Read 04CDH");
		return 0;
	}
	return 0xff;
}

std::vector <std::string> TownsCDROM::GetStatusText(void) const
{
	std::vector <std::string> text;

	text.push_back("");
	text.back()+="Last Command:"+cpputil::Ubtox(state.cmd);

	switch(state.cmd&0x9F)
	{
	case CDCMD_SEEK://       0x00,
		text.back()+="(SEEK)";
		break;
	case CDCMD_MODE2READ://  0x01,
		text.back()+="(MODE2READ)";
		break;
	case CDCMD_MODE1READ://  0x02,
		text.back()+="(MODE1READ)";
		break;
	case CDCMD_RAWREAD://    0x03,
		text.back()+="(RAWREAD)";
		break;
	case CDCMD_CDDAPLAY://   0x04,
		text.back()+="(CDDAPLAY)";
		break;
	case CDCMD_TOCREAD://    0x05,
		text.back()+="(TOCREAD)";
		break;
	case CDCMD_SUBQREAD://   0x06,
		text.back()+="(SUBQREAD)";
		break;
	case CDCMD_UNKNOWN1://   0x1F, // NOP and requst status? I guess?
		text.back()+="(UNKNOWN1)";
		break;

	case CDCMD_SETSTATE://   0x80,
		text.back()+="(SETSTATE)";
		break;
	case CDCMD_CDDASET://    0x81,
		text.back()+="(CDDASET)";
		break;
	case CDCMD_CDDASTOP://   0x84,
		text.back()+="(CDDASTOP)";
		break;
	case CDCMD_CDDAPAUSE://  0x85,
		text.back()+="(CDDAPAUSE)";
		break;
	case CDCMD_UNKNOWN2://   0x86,
		text.back()+="(UNKNOWN2)";
		break;
	case CDCMD_CDDARESUME:// 0x87,
		text.back()+="(CDDARESUME)";
		break;
	}
	if(0!=(state.cmd&0x40))
	{
		text.back()+="+IRQ";
	}
	if(0!=(state.cmd&0x20))
	{
		text.back()+="+REQSTA";
	}


	text.push_back("Param Queue (Towns->CD):");
	for(int i=0; i<state.nParamQueue; ++i)
	{
		text.back()+=cpputil::Ubtox(state.paramQueue[i])+" ";
	}

	text.push_back("Param Queue (Copied):");
	for(int i=0; i<sizeof(state.paramQueueCopy)/sizeof(state.paramQueueCopy[0]); ++i)
	{
		text.back()+=cpputil::Ubtox(state.paramQueueCopy[i])+" ";
	}

	text.push_back("Status Queue (CD->Towns):");
	for(auto d : state.statusQueue)
	{
		text.back()+=cpputil::Ubtox(d)+" ";
	}

	text.push_back("DMA Transfer:");
	text.back()+=(true==state.DMATransfer ? "1" : "0");
	text.back()+=("  CPU Transfer:");
	text.back()+=(true==state.CPUTransfer ? "1" : "0");

	text.push_back("");
	text.back()+="SIRQ:";
	text.back()+=(true==state.SIRQ ? "1" : "0");
	text.back()+="  DEI:";
	text.back()+=(true==state.DEI ? "1" : "0");
	text.back()+="  STSF:";
	text.back()+=(true==state.STSF ? "1" : "0");
	text.back()+="  DTSF:";
	text.back()+=(true==state.DTSF ? "1" : "0");
	text.back()+="  DRY:";
	text.back()+=(true==state.DRY ? "1" : "0");
	text.back()+="  enableSIRQ:";
	text.back()+=(true==state.enableSIRQ ? "1" : "0");
	text.back()+="  enableDEI:";
	text.back()+=(true==state.enableDEI ? "1" : "0");

	return text;
}

unsigned int TownsCDROM::LoadDiscImage(const std::string &fName)
{
	auto return_value=state.GetDisc().Open(fName);
	state.discChanged=true;
	return return_value;
}
void TownsCDROM::ExecuteCDROMCommand(void)
{
	switch(state.cmd&0x9F)
	{
	case CDCMD_SEEK://       0x00,
		if(0x20&state.cmd)
		{
			SetStatusDriveNotReadyOrDiscChangedOrNoError();
		}
		break;
	case CDCMD_MODE2READ://  0x01,
		std::cout << "CDROM Command " << cpputil::Ubtox(state.cmd) << " not implemented yet." << std::endl;
		break;
	case CDCMD_MODE1READ://  0x02,
		CopyParameterQueue();
		townsPtr->ScheduleDeviceCallBack(*this,townsPtr->state.townsTime+READ_SECTOR_TIME);
		break;
	case CDCMD_RAWREAD://    0x03,
		std::cout << "CDROM Command " << cpputil::Ubtox(state.cmd) << " not implemented yet." << std::endl;
		break;
	case CDCMD_CDDAPLAY://   0x04,
		std::cout << "CDROM Command " << cpputil::Ubtox(state.cmd) << " not implemented yet." << std::endl;
		break;
	case CDCMD_TOCREAD://    0x05,
		if(0x20&state.cmd)
		{
			SetStatusQueueForTOC();
		}
		break;
	case CDCMD_SUBQREAD://   0x06,
		std::cout << "CDROM Command " << cpputil::Ubtox(state.cmd) << " not implemented yet." << std::endl;
		break;
	case CDCMD_UNKNOWN1://   0x1F, // NOP and requst status? I guess?
		std::cout << "CDROM Command " << cpputil::Ubtox(state.cmd) << " not implemented yet." << std::endl;
		break;

	case CDCMD_SETSTATE://   0x80,
		if(0x20&state.cmd)
		{
			SetStatusDriveNotReadyOrDiscChangedOrNoError();
		}
		break;
	case CDCMD_CDDASET://    0x81,
		std::cout << "CDROM Command " << cpputil::Ubtox(state.cmd) << " not implemented yet." << std::endl;
		break;
	case CDCMD_CDDASTOP://   0x84,
		std::cout << "CDROM Command " << cpputil::Ubtox(state.cmd) << " not implemented yet." << std::endl;
		break;
	case CDCMD_CDDAPAUSE://  0x85,
		std::cout << "CDROM Command " << cpputil::Ubtox(state.cmd) << " not implemented yet." << std::endl;
		break;
	case CDCMD_UNKNOWN2://   0x86,
		std::cout << "CDROM Command " << cpputil::Ubtox(state.cmd) << " not implemented yet." << std::endl;
		break;
	case CDCMD_CDDARESUME:// 0x87,
		std::cout << "CDROM Command " << cpputil::Ubtox(state.cmd) << " not implemented yet." << std::endl;
		break;
	}

	state.cmdReceived=false;
	state.nParamQueue=0;

	// Tentatively Drive Not Ready.
}
/* virtual */ void TownsCDROM::RunScheduledTask(unsigned long long int townsTime)
{
}
void TownsCDROM::SetStatusDriveNotReadyOrDiscChangedOrNoError(void)
{
	if(true!=SetStatusDriveNotReadyOrDiscChanged())
	{
		SetStatusNoError();
	}
}
bool TownsCDROM::SetStatusDriveNotReadyOrDiscChanged(void)
{
	if(0==state.GetDisc().GetNumTracks())
	{
		SetStatusDriveNotReady();
		return true;
	}
	else if(true==state.discChanged)
	{
		SetStatusDiscChanged();
		state.discChanged=false;
		return true;
	}
	return false;
}
void TownsCDROM::SetStatusNoError(void)
{
	state.PushStatusQueue(0,0,0,0);
}
void TownsCDROM::SetStatusDriveNotReady(void)
{
	state.PushStatusQueue(0x21,7,0,0);
}
void TownsCDROM::SetStatusDiscChanged(void)
{
	state.PushStatusQueue(0x21,8,0,0);
}
void TownsCDROM::SetStatusQueueForTOC(void)
{
	if(true==SetStatusDriveNotReadyOrDiscChanged())
	{
		return;
	}
	auto &disc=state.GetDisc();

	state.statusQueue.clear();

	SetStatusNoError();

	state.PushStatusQueue(0x16,0,0,0);
	state.PushStatusQueue(0x17,1,0,0);

	state.PushStatusQueue(0x16,0,0,0);
	state.PushStatusQueue(0x17,disc.GetNumTracks(),0,0);

	auto length=DiscImage::HSGtoMSF(disc.GetNumSectors()+DiscImage::HSG_BASE-1); // Prob -1
	state.PushStatusQueue(0x16,0,0,0);
	state.PushStatusQueue(0x17,
	                      DiscImage::BinToBCD(length.min),
	                      DiscImage::BinToBCD(length.sec),
	                      DiscImage::BinToBCD(length.frm));

	for(auto &trk : disc.GetTracks())
	{
		unsigned char secondByte=(trk.trackType==DiscImage::TRACK_AUDIO ? 0 : 0x40);
		state.PushStatusQueue(0x16,secondByte,0,0);

		auto HSG=DiscImage::MSFtoHSG(trk.start);
		HSG+=DiscImage::HSG_BASE;
		auto MSF=DiscImage::HSGtoMSF(HSG);

		state.PushStatusQueue(0x17,
	                      DiscImage::BinToBCD(MSF.min),
	                      DiscImage::BinToBCD(MSF.sec),
	                      DiscImage::BinToBCD(MSF.frm));
	}
}
void TownsCDROM::CopyParameterQueue(void)
{
	for(int i=0; i<sizeof(state.paramQueue)/sizeof(state.paramQueue[0]); ++i)
	{
		state.paramQueueCopy[i]=state.paramQueue[i];
	}
}
