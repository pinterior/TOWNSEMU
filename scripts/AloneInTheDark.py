import os
import subprocess
import shutil
import sys

import build

TOWNSTYPE="MX"

THISFILE=os.path.realpath(__file__)
THISDIR=os.path.dirname(THISFILE)
BUILDDIR=os.path.join(THISDIR,"..","build")
SRCDIR=os.path.join(THISDIR,"..","src")
ROMDIR=os.path.join(THISDIR,"..","..","TOWNSEMU_TEST","ROM_"+TOWNSTYPE)
DISKDIR=os.path.join(THISDIR,"..","..","TOWNSEMU_TEST","DISKIMG")



def ExeExtension():
	if sys.platform.startswith('win'):
		return ".exe"
	else:
		return ""


def Run(argv):
	os.chdir(BUILDDIR)
	subprocess.Popen([
		"./main_cui/Tsugaru_CUI"+ExeExtension(),
		ROMDIR,
		"-CD",
		"E:/ISOImage/FM-TOWNS/AloneInTheDark/ALONE.CUE",
		"-FD0",
		os.path.join(DISKDIR,"ALONE.BIN"),
		"-GAMEPORT0",
		"KEY",
		"-SYM",
		"../../TOWNSEMU_TEST/symtables/ALONE_"+TOWNSTYPE+".txt",
		"-HD0",
		os.path.join(DISKDIR,"hddimage.bin"),
		"-CMOS",
		"../testdata/CMOS.bin",
		"-DONTAUTOSAVECMOS",
		"-SCALE",
		"160",
		# "-DEBUG",
		# "-PAUSE",
	]+argv).wait()



if __name__=="__main__":
	build.Run()
	Run(sys.argv[1:])
