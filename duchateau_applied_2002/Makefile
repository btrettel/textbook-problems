.POSIX:

duchateau102.exe: duchateau102.f90
	wine elf90 -npause -fullwarn -winconsole duchateau102.f90 -out duchateau102.exe

test: duchateau102.exe
	wine duchateau102.exe

.PHONY: clean
clean:
	rm -rfv *.exe *.map *.obj
