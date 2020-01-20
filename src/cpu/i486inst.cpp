#include <iostream>

#include "cpputil.h"
#include "i486.h"
#include "i486inst.h"



bool i486DX::OpCodeNeedsOneMoreByte(unsigned int firstByte) const
{
	switch(firstByte)
	{
	case I486_OPCODE_NEED_SECOND_BYTE:
		return true;
	}
	return false;
}


i486DX::Instruction i486DX::FetchInstruction(const SegmentRegister seg,unsigned int offset,const Memory &mem) const
{
	Instruction inst;
	inst.Clear();
	if(true==IsInRealMode())
	{
		inst.operandSize=16;
		inst.addressSize=16;
	}
	else
	{
		// Default operandSize and addressSize depends on the D flag of the segment descriptor.
		Abort("Protected mode not supported yet.");
	}

	// Question: Do prefixes need to be in the specific order INST_PREFIX->ADDRSIZE_OVERRIDE->OPSIZE_OVERRIDE->SEG_OVERRIDE?

	unsigned int lastByte=FetchByte(seg,offset+inst.numBytes++,mem);
	for(;;) // While looking at prefixes.
	{
		switch(lastByte)
		{
		case INST_PREFIX_REP: // REP/REPE/REPZ
		case INST_PREFIX_REPNE:
		case INST_PREFIX_LOCK:
			inst.instPrefix=lastByte;
			break;

		case SEG_OVERRIDE_CS:
		case SEG_OVERRIDE_SS:
		case SEG_OVERRIDE_DS:
		case SEG_OVERRIDE_ES:
		case SEG_OVERRIDE_FS:
		case SEG_OVERRIDE_GS:
			inst.segOverride=lastByte;
			break;

		case OPSIZE_OVERRIDE:
			inst.operandSize^=48;
			break;
		case ADDRSIZE_OVERRIDE:
			inst.addressSize^=48;
			break;
		default:
			goto PREFIX_DONE;
		}
		lastByte=FetchByte(seg,offset+inst.numBytes++,mem);
	}
PREFIX_DONE:
	inst.opCode=lastByte;
	if(true==OpCodeNeedsOneMoreByte(inst.opCode))
	{
		lastByte=FetchByte(seg,offset+inst.numBytes++,mem);
		inst.opCode|=(lastByte<<8);
	}

	FetchOperand(inst,seg,offset+inst.numBytes,mem);

	return inst;
}

unsigned int i486DX::FetchOperand8(Instruction &inst,SegmentRegister seg,unsigned int offset,const Memory &mem) const
{
	auto byte=FetchByte(seg,offset++,mem);
	inst.operand[inst.operandLen++]=byte;
	++inst.numBytes;
	return 1;
}
unsigned int i486DX::FetchOperand16(Instruction &inst,SegmentRegister seg,unsigned int offset,const Memory &mem) const
{
	unsigned int byte[2];
	byte[0]=FetchByte(seg,offset++,mem);
	byte[1]=FetchByte(seg,offset++,mem);

	inst.operand[inst.operandLen++]=byte[0];
	inst.operand[inst.operandLen++]=byte[1];
	inst.numBytes+=2;
	return 2;
}
unsigned int i486DX::FetchOperand32(Instruction &inst,SegmentRegister seg,unsigned int offset,const Memory &mem) const
{
	unsigned int byte[4];
	byte[0]=FetchByte(seg,offset++,mem);
	byte[1]=FetchByte(seg,offset++,mem);
	byte[2]=FetchByte(seg,offset++,mem);
	byte[3]=FetchByte(seg,offset++,mem);
	inst.operand[inst.operandLen++]=byte[0];
	inst.operand[inst.operandLen++]=byte[1];
	inst.operand[inst.operandLen++]=byte[2];
	inst.operand[inst.operandLen++]=byte[3];
	inst.numBytes+=4;
	return 4;
}

unsigned int i486DX::FetchOperand16or32(Instruction &inst,SegmentRegister seg,unsigned int offset,const Memory &mem) const
{
	if(16==inst.operandSize)
	{
		return FetchOperand16(inst,seg,offset,mem);
	}
	else // if(32==inst.operandSize)
	{
		return FetchOperand32(inst,seg,offset,mem);
	}
}

unsigned int i486DX::FetchOperandRM(Instruction &inst,SegmentRegister seg,unsigned int offset,const Memory &mem) const
{
	unsigned int numBytesFetched=1;
	auto MODR_M=FetchOperand8(inst,seg,offset,mem);
	auto MOD=(MODR_M>>6)&3;
	auto R_M=(MODR_M)&7;

	// [1] Table 26-1, 26-2, 26-3, pp. 26-4,26-5,26-6
	if(16==inst.addressSize)
	{
		if(0b00==MOD && 0b110==R_M) // disp16
		{
			numBytesFetched+=FetchOperand16(inst,seg,offset,mem);
		}
		else if(0b01==MOD)
		{
			numBytesFetched+=FetchOperand8(inst,seg,offset,mem);
		}
		else if(0b10==MOD)
		{
			numBytesFetched+=FetchOperand16(inst,seg,offset,mem);
		}
	}
	else // if(32==inst.addressSize)
	{
		if(0b00==MOD)
		{
			if(0b100==R_M) // SIB
			{
				FetchOperand8(inst,seg,offset,mem);
				++numBytesFetched;
				++offset;

				auto SIB=inst.operand[inst.operandLen-1];
				auto BASE=(SIB&7);
				// Special case MOD=0b00 && BASE==5 [1] Table 26-4 pp.26-7
				// No base, [disp32+scaled_index]

				numBytesFetched+=FetchOperand32(inst,seg,offset,mem);
			}
			else if(0b101==R_M) // disp32
			{
				numBytesFetched+=FetchOperand32(inst,seg,offset,mem);
			}
		}
		else if(0b01==MOD)
		{
			if(0b100==R_M) // SIB+disp8
			{
				FetchOperand8(inst,seg,offset,mem);
				++numBytesFetched;
				++offset;
			}
			FetchOperand8(inst,seg,offset,mem);
			++numBytesFetched;
		}
		else if(0b10==MOD)
		{
			if(0b100==R_M) // SIB+disp32
			{
				FetchOperand8(inst,seg,offset,mem);
				++numBytesFetched;
				++offset;
			}
			numBytesFetched+=FetchOperand32(inst,seg,offset,mem);
		}
	}

	return numBytesFetched;
}

void i486DX::FetchOperand(Instruction &inst,SegmentRegister seg,int offset,const Memory &mem) const
{
	switch(inst.opCode)
	{
	case I486_OPCODE_CLD:
	case I486_OPCODE_CLI:
		break;

	case I486_OPCODE_DEC_R_M8:
	case I486_OPCODE_DEC_R_M:
		FetchOperandRM(inst,seg,offset,mem);
		break;
	case I486_OPCODE_DEC_EAX:
	case I486_OPCODE_DEC_ECX:
	case I486_OPCODE_DEC_EDX:
	case I486_OPCODE_DEC_EBX:
	case I486_OPCODE_DEC_ESP:
	case I486_OPCODE_DEC_EBP:
	case I486_OPCODE_DEC_ESI:
	case I486_OPCODE_DEC_EDI:
		break;

	case I486_OPCODE_JMP_FAR:
		offset+=FetchOperand16or32(inst,seg,offset,mem);
		FetchOperand16(inst,seg,offset,mem);
		break;

	case I486_OPCODE_MOV_FROM_R8: //      0x88,
		// Example:  88 4c ff        MOV CL,[SI-1]     In Real Mode
		// Example:  88 10           MOV DL,[BX+SI]    In Real Mode
		// Example:  88 36 21 00     MOV DH,[021H]     In Real Mode
		// Example:  67 88 26 61 10  MOV [1061],AH     In Protected Mode -> disp16 may become disp32, and vise-versa

		// Example:  8D 04 C1        LEA EAX,[ECX+EAX*8] In Protected Mode
		// Example:  8D 04 41        LEA EAX,[ECX+EAX*2] In Protected Mode
	case I486_OPCODE_MOV_FROM_R: //       0x89, // 16/32 depends on OPSIZE_OVERRIDE
		// Example:  89 26 3e 00     MOV [003EH],SP
	case I486_OPCODE_MOV_TO_R8: //        0x8A,
		// Example:  8a 0e 16 00     MOV CL,[0016H]
	case I486_OPCODE_MOV_TO_R: //         0x8B, // 16/32 depends on OPSIZE_OVERRIDE
		FetchOperandRM(inst,seg,offset,mem);
		break;


	case I486_OPCODE_MOV_FROM_SEG: //     0x8C,
		// Example:  8c c6           MOV SI,ES
		// Sreg: ES=0, CS=1, SS=2, DS=3, FD=4, GS=5 (OPCODE part of MODR_M)  [1] pp.26-10
	case I486_OPCODE_MOV_TO_SEG: //       0x8E,
		FetchOperand8(inst,seg,offset,mem);
		break;

	case I486_OPCODE_MOV_M_TO_AL: //      0xA0,
	case I486_OPCODE_MOV_M_TO_EAX: //     0xA1, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_M_FROM_AL: //    0xA2,
	case I486_OPCODE_MOV_M_FROM_EAX: //   0xA3, // 16/32 depends on OPSIZE_OVERRIDE
		FetchOperandRM(inst,seg,offset,mem);
		break;

	case I486_OPCODE_MOV_I8_TO_AL: //     0xB0,
	case I486_OPCODE_MOV_I8_TO_CL: //     0xB1,
	case I486_OPCODE_MOV_I8_TO_DL: //     0xB2,
	case I486_OPCODE_MOV_I8_TO_BL: //     0xB3,
	case I486_OPCODE_MOV_I8_TO_AH: //     0xB4,
	case I486_OPCODE_MOV_I8_TO_CH: //     0xB5,
	case I486_OPCODE_MOV_I8_TO_DH: //     0xB6,
	case I486_OPCODE_MOV_I8_TO_BH: //     0xB7,
		FetchOperand8(inst,seg,offset,mem);
		break;
	case I486_OPCODE_MOV_I_TO_EAX: //   0xB8, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_I_TO_ECX: //   0xB9, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_I_TO_EDX: //   0xBA, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_I_TO_EBX: //   0xBB, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_I_TO_ESP: //   0xBC, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_I_TO_EBP: //   0xBD, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_I_TO_ESI: //   0xBE, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_I_TO_EDI: //   0xBF, // 16/32 depends on OPSIZE_OVERRIDE
		FetchOperand16or32(inst,seg,offset,mem);
		break;
	case I486_OPCODE_MOV_I8_TO_RM8: //    0xC6,
		offset+=FetchOperandRM(inst,seg,offset,mem);
		FetchOperand8(inst,seg,offset,mem);
		break;
	case I486_OPCODE_MOV_I_TO_RM: //      0xC7, // 16/32 depends on OPSIZE_OVERRIDE
		offset+=FetchOperandRM(inst,seg,offset,mem);
		FetchOperand16or32(inst,seg,offset,mem);
		break;

	case I486_OPCODE_OUT_I8_AL: //        0xE6,
	case I486_OPCODE_OUT_I8_A: //         0xE7,
		FetchOperand8(inst,seg,offset,mem);
		break;
	case I486_OPCODE_OUT_DX_AL: //        0xEE,
	case I486_OPCODE_OUT_DX_A: //         0xEF,
		break;

	case I486_OPCODE_XOR_AL_FROM_I8:
		FetchOperand8(inst,seg,offset,mem);
		break;
	case I486_OPCODE_XOR_A_FROM_I:
		FetchOperand16or32(inst,seg,offset,mem);
		break;
	case I486_OPCODE_XOR_RM8_FROM_I8:
		offset+=FetchOperandRM(inst,seg,offset,mem);
		FetchOperand8(inst,seg,offset,mem);
		break;
	case I486_OPCODE_XOR_R_FROM_I:
		offset+=FetchOperandRM(inst,seg,offset,mem);
		FetchOperand16or32(inst,seg,offset,mem);
		break;
	case I486_OPCODE_XOR_RM_FROM_SXI8:
		offset+=FetchOperandRM(inst,seg,offset,mem);
		FetchOperand8(inst,seg,offset,mem);
		break;
	case I486_OPCODE_XOR_RM8_FROM_R8:
	case I486_OPCODE_XOR_RM_FROM_R:
	case I486_OPCODE_XOR_R8_FROM_RM8:
	case I486_OPCODE_XOR_R_FROM_RM:
		FetchOperandRM(inst,seg,offset,mem);
		break;

	default:
		// Undefined operand, or probably not implemented yet.
		break;
	}
}

void i486DX::Instruction::DecodeOperand(int addressSize,int operandSize,Operand &op1,Operand &op2) const
{
	op1.Clear();
	op2.Clear();

	switch(opCode)
	{
	case I486_OPCODE_CLD:
	case I486_OPCODE_CLI:
		break;


	case I486_OPCODE_DEC_R_M8:
		op1.Decode(addressSize,8,operand);
		break;
	case I486_OPCODE_DEC_R_M:
		op1.Decode(addressSize,operandSize,operand);
		break;

	case I486_OPCODE_DEC_EAX:
	case I486_OPCODE_DEC_ECX:
	case I486_OPCODE_DEC_EDX:
	case I486_OPCODE_DEC_EBX:
	case I486_OPCODE_DEC_ESP:
	case I486_OPCODE_DEC_EBP:
	case I486_OPCODE_DEC_ESI:
	case I486_OPCODE_DEC_EDI:
		op1.MakeByRegisterNumber(operandSize,opCode&7);
		break;


	case I486_OPCODE_JMP_FAR:
		op1.DecodeFarAddr(addressSize,operandSize,operand);
		break;

	case I486_OPCODE_MOV_FROM_R8: //      0x88,
		op1.Decode(addressSize,8,operand);
		op2.DecodeMODR_MForRegister(8,operand[0]);
		break;
	case I486_OPCODE_MOV_FROM_R: //       0x89, // 16/32 depends on OPSIZE_OVERRIDE
		op1.Decode(addressSize,operandSize,operand);
		op2.DecodeMODR_MForRegister(operandSize,operand[0]);
		break;
	case I486_OPCODE_MOV_TO_R8: //        0x8A,
		op2.Decode(addressSize,8,operand);
		op1.DecodeMODR_MForRegister(8,operand[0]);
		break;
	case I486_OPCODE_MOV_TO_R: //         0x8B, // 16/32 depends on OPSIZE_OVERRIDE
		op2.Decode(addressSize,operandSize,operand);
		op1.DecodeMODR_MForRegister(operandSize,operand[0]);
		break;
	case I486_OPCODE_MOV_FROM_SEG: //     0x8C,
		op1.Decode(addressSize,operandSize,operand);
		op2.DecodeMODR_MForSegmentRegister(operand[0]);
		break;
	case I486_OPCODE_MOV_TO_SEG: //       0x8E,
		op2.Decode(addressSize,operandSize,operand);
		op1.DecodeMODR_MForSegmentRegister(operand[0]);
		break;
	case I486_OPCODE_MOV_M_TO_AL: //      0xA0,
		op2.Decode(addressSize,operandSize,operand);
		op1.MakeByRegisterNumber(8,REG_AL-REG_8BIT_REG_BASE);
		break;
	case I486_OPCODE_MOV_M_TO_EAX: //     0xA1, // 16/32 depends on OPSIZE_OVERRIDE
		op2.Decode(addressSize,operandSize,operand);
		op1.MakeByRegisterNumber(operandSize,REG_AL-REG_8BIT_REG_BASE);
		break;
	case I486_OPCODE_MOV_M_FROM_AL: //    0xA2,
		op1.Decode(addressSize,operandSize,operand);
		op2.MakeByRegisterNumber(8,REG_AL-REG_8BIT_REG_BASE);
		break;
	case I486_OPCODE_MOV_M_FROM_EAX: //   0xA3, // 16/32 depends on OPSIZE_OVERRIDE
		op1.Decode(addressSize,operandSize,operand);
		op2.MakeByRegisterNumber(operandSize,REG_AL-REG_8BIT_REG_BASE);
		break;
	case I486_OPCODE_MOV_I8_TO_AL: //     0xB0,
	case I486_OPCODE_MOV_I8_TO_CL: //     0xB1,
	case I486_OPCODE_MOV_I8_TO_DL: //     0xB2,
	case I486_OPCODE_MOV_I8_TO_BL: //     0xB3,
	case I486_OPCODE_MOV_I8_TO_AH: //     0xB4,
	case I486_OPCODE_MOV_I8_TO_CH: //     0xB5,
	case I486_OPCODE_MOV_I8_TO_DH: //     0xB6,
	case I486_OPCODE_MOV_I8_TO_BH: //     0xB7,
		op1.MakeByRegisterNumber(8,opCode&7);
		op2.MakeImm8(*this);
		break;
	case I486_OPCODE_MOV_I_TO_EAX: //     0xB8, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_I_TO_ECX: //     0xB9, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_I_TO_EDX: //     0xBA, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_I_TO_EBX: //     0xBB, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_I_TO_ESP: //     0xBC, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_I_TO_EBP: //     0xBD, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_I_TO_ESI: //     0xBE, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_I_TO_EDI: //     0xBF, // 16/32 depends on OPSIZE_OVERRIDE
		op1.MakeByRegisterNumber(operandSize,opCode&7);
		if(16==operandSize)
		{
			op2.MakeImm16(*this);
		}
		else
		{
			op2.MakeImm32(*this);
		}
		break;
	case I486_OPCODE_MOV_I8_TO_RM8: //    0xC6,
		op1.Decode(addressSize,8,operand);
		op2.MakeImm8(*this);
		break;
	case I486_OPCODE_MOV_I_TO_RM: //      0xC7, // 16/32 depends on OPSIZE_OVERRIDE
		op1.Decode(addressSize,operandSize,operand);
		op2.MakeImm16or32(*this,operandSize);
		break;

	case I486_OPCODE_OUT_I8_AL: //        0xE6,
	case I486_OPCODE_OUT_I8_A: //         0xE7,
		op1.MakeImm8(*this);
		break;
	case I486_OPCODE_OUT_DX_AL: //        0xEE,
	case I486_OPCODE_OUT_DX_A: //         0xEF,
		break;


	case I486_OPCODE_XOR_AL_FROM_I8:
		op1.MakeImm8(*this);
		break;
	case I486_OPCODE_XOR_A_FROM_I:
		op1.MakeImm16or32(*this,operandSize);
		break;
	case I486_OPCODE_XOR_RM8_FROM_I8:
		op1.Decode(addressSize,8,operand);
		op2.MakeImm8(*this);
		break;
	case I486_OPCODE_XOR_R_FROM_I:
		op1.Decode(addressSize,operandSize,operand);
		op2.MakeImm16or32(*this,operandSize);
		break;
	case I486_OPCODE_XOR_RM_FROM_SXI8:
		op1.Decode(addressSize,operandSize,operand);
		op2.MakeImm8(*this);
		if(16==operandSize)
		{
			op2.SignExtendImm(OPER_IMM16);
		}
		else
		{
			op2.SignExtendImm(OPER_IMM32);
		}
		break;
	case I486_OPCODE_XOR_RM8_FROM_R8:
		op2.DecodeMODR_MForRegister(8,operand[0]);
		op1.Decode(addressSize,8,operand);
		break;
	case I486_OPCODE_XOR_RM_FROM_R:
		op2.DecodeMODR_MForRegister(operandSize,operand[0]);
		op1.Decode(addressSize,operandSize,operand);
		break;
	case I486_OPCODE_XOR_R8_FROM_RM8:
		op1.DecodeMODR_MForRegister(8,operand[0]);
		op2.Decode(addressSize,8,operand);
		break;
	case I486_OPCODE_XOR_R_FROM_RM:
		op1.DecodeMODR_MForRegister(operandSize,operand[0]);
		op2.Decode(addressSize,operandSize,operand);
		break;
	}
}

std::string i486DX::Instruction::Disassemble(SegmentRegister cs,unsigned int eip) const
{
	std::string disasm;
	Operand op1,op2;
	std::string op1SizeQual,op2SizeQual;
	std::string op1SegQual,op2SegQual;

	DecodeOperand(addressSize,operandSize,op1,op2);

	switch(opCode)
	{
	case I486_OPCODE_CLD:
		disasm="CLD";
		break;
	case I486_OPCODE_CLI:
		disasm="CLI";
		break;


	case I486_OPCODE_DEC_R_M8:
		disasm=DisassembleTypicalOneOperand("DEC",op1,8);
		break;
	case I486_OPCODE_DEC_R_M:
		disasm=DisassembleTypicalOneOperand("DEC",op1,operandSize);
		break;
	case I486_OPCODE_DEC_EAX:
	case I486_OPCODE_DEC_ECX:
	case I486_OPCODE_DEC_EDX:
	case I486_OPCODE_DEC_EBX:
	case I486_OPCODE_DEC_ESP:
	case I486_OPCODE_DEC_EBP:
	case I486_OPCODE_DEC_ESI:
	case I486_OPCODE_DEC_EDI:
		disasm="DEC";
		cpputil::ExtendString(disasm,8);
		if(16==operandSize)
		{
			disasm+=Reg16[opCode&7];
		}
		else
		{
			disasm+=Reg32[opCode&7];
		}
		break;


	case I486_OPCODE_JMP_FAR:
		disasm="JMP";
		cpputil::ExtendString(disasm,8);
		disasm+=op1.Disassemble();
		break;


	case I486_OPCODE_MOV_FROM_R8: //      0x88,
	case I486_OPCODE_MOV_FROM_R: //       0x89, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_TO_R8: //        0x8A,
	case I486_OPCODE_MOV_TO_R: //         0x8B, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_FROM_SEG: //     0x8C,
	case I486_OPCODE_MOV_TO_SEG: //       0x8E,
	case I486_OPCODE_MOV_M_TO_AL: //      0xA0,
	case I486_OPCODE_MOV_M_TO_EAX: //     0xA1, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_M_FROM_AL: //    0xA2,
	case I486_OPCODE_MOV_M_FROM_EAX: //   0xA3, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_I8_TO_AL: //     0xB0,
	case I486_OPCODE_MOV_I8_TO_CL: //     0xB1,
	case I486_OPCODE_MOV_I8_TO_DL: //     0xB2,
	case I486_OPCODE_MOV_I8_TO_BL: //     0xB3,
	case I486_OPCODE_MOV_I8_TO_AH: //     0xB4,
	case I486_OPCODE_MOV_I8_TO_CH: //     0xB5,
	case I486_OPCODE_MOV_I8_TO_DH: //     0xB6,
	case I486_OPCODE_MOV_I8_TO_BH: //     0xB7,
	case I486_OPCODE_MOV_I_TO_EAX: //     0xB8, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_I_TO_ECX: //     0xB9, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_I_TO_EDX: //     0xBA, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_I_TO_EBX: //     0xBB, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_I_TO_ESP: //     0xBC, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_I_TO_EBP: //     0xBD, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_I_TO_ESI: //     0xBE, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_I_TO_EDI: //     0xBF, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_I8_TO_RM8: //    0xC6,
	case I486_OPCODE_MOV_I_TO_RM: //      0xC7, // 16/32 depends on OPSIZE_OVERRIDE
		disasm=DisassembleTypicalTwoOperands("MOV",op1,op2);
		break;

	case I486_OPCODE_OUT_I8_AL: //        0xE6,
		disasm="OUT";
		cpputil::ExtendString(disasm,8);
		disasm+=op1.Disassemble();
		disasm+=",AL";
		break;
	case I486_OPCODE_OUT_I8_A: //         0xE7,
		disasm="OUT";
		cpputil::ExtendString(disasm,8);
		disasm+=op1.Disassemble();
		if(16==operandSize)
		{
			disasm+=",AX";
		}
		else
		{
			disasm+=",EAX";
		}
		break;
	case I486_OPCODE_OUT_DX_AL: //        0xEE,
		disasm="OUT     DX,AL";
		break;
	case I486_OPCODE_OUT_DX_A: //         0xEF,
		if(16==operandSize)
		{
			disasm="OUT     DX,AX";
		}
		else
		{
			disasm="OUT     DX,EAX";
		}
		break;


	case I486_OPCODE_XOR_AL_FROM_I8:
		disasm="XOR     AL,"+op1.Disassemble();
		break;
	case I486_OPCODE_XOR_A_FROM_I:
		if(16==operandSize)
		{
			disasm="XOR     AX,"+op1.Disassemble();;
		}
		else
		{
			disasm="XOR     EAX,"+op1.Disassemble();;
		}
		break;
	case I486_OPCODE_XOR_RM8_FROM_I8:
	case I486_OPCODE_XOR_R_FROM_I:
	case I486_OPCODE_XOR_RM_FROM_SXI8:
	case I486_OPCODE_XOR_RM8_FROM_R8:
	case I486_OPCODE_XOR_RM_FROM_R:
	case I486_OPCODE_XOR_R8_FROM_RM8:
	case I486_OPCODE_XOR_R_FROM_RM:
		disasm=DisassembleTypicalTwoOperands("XOR",op1,op2);
		break;
	}

	return disasm;
}

std::string i486DX::Instruction::DisassembleTypicalOneOperand(std::string inst,const Operand &op,int operandSize) const
{
	auto sizeQual=i486DX::Operand::GetSizeQualifierToDisassembly(op,operandSize);
	auto segQual=i486DX::Operand::GetSegmentQualifierToDisassembly(segOverride,op);
	auto disasm=inst;
	cpputil::ExtendString(disasm,8);
	disasm+=sizeQual+segQual+op.Disassemble();
	return disasm;
}

std::string i486DX::Instruction::DisassembleTypicalTwoOperands(std::string inst,const Operand &op1,const Operand &op2) const
{
	std::string disasm=inst,op1SizeQual,op2SizeQual,op1SegQual,op2SegQual;
	cpputil::ExtendString(disasm,8);

	i486DX::Operand::GetSizeQualifierToDisassembly(op1SizeQual,op2SizeQual,op1,op2);
	op1SegQual=i486DX::Operand::GetSegmentQualifierToDisassembly(segOverride,op1);
	op2SegQual=i486DX::Operand::GetSegmentQualifierToDisassembly(segOverride,op2);
	disasm+=op1SizeQual+op1SegQual+op1.Disassemble();
	disasm.push_back(',');
	disasm+=op2SizeQual+op2SegQual+op2.Disassemble();

	return disasm;
}

unsigned int i486DX::Instruction::GetUimm8(void) const
{
	return operand[operandLen-1];
}
unsigned int i486DX::Instruction::GetUimm16(void) const
{
	return cpputil::GetWord(operand+operandLen-2);
}
unsigned int i486DX::Instruction::GetUimm32(void) const
{
	return cpputil::GetDword(operand+operandLen-4);
}

/* static */ std::string i486DX::Get8BitRegisterNameFromMODR_M(unsigned char MOD_RM)
{
	auto REG_OPCODE=((MOD_RM>>3)&7);
	return Reg8[REG_OPCODE];
}
/* static */ std::string i486DX::Get16BitRegisterNameFromMODR_M(unsigned char MOD_RM)
{
	auto REG_OPCODE=((MOD_RM>>3)&7);
	return Reg16[REG_OPCODE];
}
/* static */ std::string i486DX::Get32BitRegisterNameFromMODR_M(unsigned char MOD_RM)
{
	auto REG_OPCODE=((MOD_RM>>3)&7);
	return Reg32[REG_OPCODE];
}
/* static */ std::string i486DX::Get16or32BitRegisterNameFromMODR_M(int dataSize,unsigned char MOD_RM)
{
	if(16==dataSize)
	{
		auto REG_OPCODE=((MOD_RM>>3)&7);
		return Reg16[REG_OPCODE];
	}
	else
	{
		auto REG_OPCODE=((MOD_RM>>3)&7);
		return Reg32[REG_OPCODE];
	}
}


unsigned int i486DX::RunOneInstruction(Memory &mem,InOut &io)
{
	auto inst=FetchInstruction(state.CS,state.EIP,mem);

	Operand op1,op2;
	inst.DecodeOperand(inst.addressSize,inst.operandSize,op1,op2);

	bool EIPChanged=false;
	unsigned int clocksPassed=0;

	switch(inst.opCode)
	{
	case I486_OPCODE_CLD:
		state.EFLAGS&=(~EFLAGS_DIRECTION);
		clocksPassed=2;
		break;
	case I486_OPCODE_CLI:
		state.EFLAGS&=(~EFLAGS_INT_ENABLE);
		clocksPassed=2;
		break;


	case I486_OPCODE_DEC_R_M8:
		{
			auto value=EvaluateOperand(mem,inst.addressSize,inst.segOverride,op1,1);
			auto i=value.GetAsDword();
			DecrementByte(i);
			value.SetDword(i);
			StoreOperandValue(op1,mem,inst.addressSize,inst.segOverride,value);
		}
		break;
	case I486_OPCODE_DEC_R_M:
		{
			auto value=EvaluateOperand(mem,inst.addressSize,inst.segOverride,op1,inst.operandSize/8);
			auto i=value.GetAsDword();
			DecrementWordOrDword(inst.operandSize,i);
			value.SetDword(i);
			StoreOperandValue(op1,mem,inst.addressSize,inst.segOverride,value);
		}
		break;
	case I486_OPCODE_DEC_EAX:
	case I486_OPCODE_DEC_ECX:
	case I486_OPCODE_DEC_EDX:
	case I486_OPCODE_DEC_EBX:
	case I486_OPCODE_DEC_ESP:
	case I486_OPCODE_DEC_EBP:
	case I486_OPCODE_DEC_ESI:
	case I486_OPCODE_DEC_EDI:
		{
			auto value=EvaluateOperand(mem,inst.addressSize,inst.segOverride,op1,op1.GetSize());
			auto i=value.GetAsDword();
			DecrementWordOrDword(inst.operandSize,i);
			value.SetDword(i);
			StoreOperandValue(op1,mem,inst.addressSize,inst.segOverride,value);
		}
		break;


	case I486_OPCODE_JMP_FAR:
		{
			switch(inst.operandSize)
			{
			case 16:
				if(true==IsInRealMode())
				{
					clocksPassed=17;
				}
				else
				{
					clocksPassed=19;
				}
				break;
			case 32:
				if(true==IsInRealMode())
				{
					clocksPassed=13;
				}
				else
				{
					clocksPassed=18;
				}
				break;
			}
			LoadSegmentRegister(state.CS,op1.seg,mem);
			state.EIP=op1.offset;
			EIPChanged=true;
		}
		break;
	case I486_OPCODE_MOV_FROM_SEG: //     0x8C,
		Move(mem,inst.addressSize,inst.segOverride,op1,op2);
		clocksPassed=3;
		break;
	case I486_OPCODE_MOV_TO_SEG: //       0x8E,
		Move(mem,inst.addressSize,inst.segOverride,op1,op2);
		if(true==IsInRealMode())
		{
			clocksPassed=3;
		}
		else
		{
			clocksPassed=9;
		}
		break;
	case I486_OPCODE_MOV_FROM_R8: //      0x88,
	case I486_OPCODE_MOV_FROM_R: //       0x89, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_TO_R8: //        0x8A,
	case I486_OPCODE_MOV_TO_R: //         0x8B, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_M_TO_AL: //      0xA0,
	case I486_OPCODE_MOV_M_TO_EAX: //     0xA1, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_M_FROM_AL: //    0xA2,
	case I486_OPCODE_MOV_M_FROM_EAX: //   0xA3, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_I8_TO_AL: //     0xB0,
	case I486_OPCODE_MOV_I8_TO_CL: //     0xB1,
	case I486_OPCODE_MOV_I8_TO_DL: //     0xB2,
	case I486_OPCODE_MOV_I8_TO_BL: //     0xB3,
	case I486_OPCODE_MOV_I8_TO_AH: //     0xB4,
	case I486_OPCODE_MOV_I8_TO_CH: //     0xB5,
	case I486_OPCODE_MOV_I8_TO_DH: //     0xB6,
	case I486_OPCODE_MOV_I8_TO_BH: //     0xB7,
	case I486_OPCODE_MOV_I_TO_EAX: //     0xB8, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_I_TO_ECX: //     0xB9, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_I_TO_EDX: //     0xBA, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_I_TO_EBX: //     0xBB, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_I_TO_ESP: //     0xBC, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_I_TO_EBP: //     0xBD, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_I_TO_ESI: //     0xBE, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_I_TO_EDI: //     0xBF, // 16/32 depends on OPSIZE_OVERRIDE
	case I486_OPCODE_MOV_I8_TO_RM8: //    0xC6,
	case I486_OPCODE_MOV_I_TO_RM: //      0xC7, // 16/32 depends on OPSIZE_OVERRIDE
		Move(mem,inst.addressSize,inst.segOverride,op1,op2);
		clocksPassed=1;
		break;

	case I486_OPCODE_OUT_I8_AL: //        0xE6,
		io.Out8(inst.operand[0],GetRegisterValue(REG_AL));
		if(true==IsInRealMode())
		{
			clocksPassed=16;
		}
		else
		{
			clocksPassed=11; // 31 if CPL>IOPL
		}
		break;
	case I486_OPCODE_OUT_I8_A: //         0xE7,
		if(16==inst.operandSize)
		{
			io.Out16(inst.operand[0],GetRegisterValue(REG_AX));
		}
		else
		{
			io.Out32(inst.operand[0],GetRegisterValue(REG_EAX));
		}
		if(true==IsInRealMode())
		{
			clocksPassed=16;
		}
		else
		{
			clocksPassed=11; // 31 if CPL>IOPL
		}
		break;
	case I486_OPCODE_OUT_DX_AL: //        0xEE,
		io.Out8(GetRegisterValue(REG_DX),GetRegisterValue(REG_AL));
		if(true==IsInRealMode())
		{
			clocksPassed=16;
		}
		else
		{
			clocksPassed=10; // 30 if CPL>IOPL
		}
		break;
	case I486_OPCODE_OUT_DX_A: //         0xEF,
		if(16==inst.operandSize)
		{
			io.Out16(GetRegisterValue(REG_DX),GetRegisterValue(REG_AX));
		}
		else
		{
			io.Out32(GetRegisterValue(REG_DX),GetRegisterValue(REG_EAX));
		}
		if(true==IsInRealMode())
		{
			clocksPassed=16;
		}
		else
		{
			clocksPassed=10; // 30 if CPL>IOPL
		}
		break;



	case I486_OPCODE_XOR_AL_FROM_I8:
		{
			auto value=EvaluateOperand(mem,inst.addressSize,inst.segOverride,op1,1);
			auto al=GetAL();
			auto v=value.GetAsDword();
			XorByte(al,v);
			SetAL(al);
		}
		break;
	case I486_OPCODE_XOR_A_FROM_I:
		if(16==inst.operandSize)
		{
			auto value=EvaluateOperand(mem,inst.addressSize,inst.segOverride,op1,inst.operandSize/8);
			auto ax=GetAX();
			auto v=value.GetAsDword();
			XorWord(ax,v);
			SetAX(ax);
		}
		else
		{
			auto value=EvaluateOperand(mem,inst.addressSize,inst.segOverride,op1,inst.operandSize/8);
			auto eax=GetEAX();
			auto v=value.GetAsDword();
			XorDword(eax,v);
			SetEAX(eax);
		}
		break;
	case I486_OPCODE_XOR_RM8_FROM_I8:
	case I486_OPCODE_XOR_RM8_FROM_R8:
	case I486_OPCODE_XOR_R8_FROM_RM8:
		{
			auto value1=EvaluateOperand(mem,inst.addressSize,inst.segOverride,op1,1);
			auto value2=EvaluateOperand(mem,inst.addressSize,inst.segOverride,op1,1);
			auto i=value1.GetAsDword();
			XorByte(i,value2.GetAsDword());
			value1.SetDword(i);
			StoreOperandValue(op1,mem,inst.addressSize,inst.segOverride,value1);
		}
		break;
	case I486_OPCODE_XOR_R_FROM_I:
	case I486_OPCODE_XOR_RM_FROM_SXI8: // Sign of op2 is already extended when decoded.
	case I486_OPCODE_XOR_RM_FROM_R:
	case I486_OPCODE_XOR_R_FROM_RM:
		{
			auto value1=EvaluateOperand(mem,inst.addressSize,inst.segOverride,op1,inst.operandSize/8);
			auto value2=EvaluateOperand(mem,inst.addressSize,inst.segOverride,op1,inst.operandSize/8);
			auto i=value1.GetAsDword();
			XorWordOrDword(inst.operandSize,i,value2.GetAsDword());
			value1.SetDword(i);
			StoreOperandValue(op1,mem,inst.addressSize,inst.segOverride,value1);
		}
		break;


	default:
		Abort("Undefined instruction or simply not supported yet.");
		break;
	}

	if(true!=EIPChanged && true!=abort)
	{
		state.EIP+=inst.numBytes;
	}

	return clocksPassed;
}