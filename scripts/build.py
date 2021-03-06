import os
import subprocess
import shutil
import sys

THISFILE=os.path.realpath(__file__)
THISDIR=os.path.dirname(THISFILE)
BUILDDIR=os.path.join(THISDIR,"..","build")
SRCDIR=os.path.join(THISDIR,"..","src")
ROMDIR=os.path.join(THISDIR,"..","testdata","ROM_MX")
DISKDIR=os.path.join(THISDIR,"..","testdata","DISKIMG")


def Make():
	if sys.platform.startswith('win'):
	    return "nmake"
	else:
	    return "make"


def Run():
	os.chdir(BUILDDIR)
	proc=subprocess.Popen([
		Make(),
	])
	proc.communicate();
	if proc.returncode!=0:
		print("Build Error!")
		quit()



if __name__=="__main__":
	Run()
