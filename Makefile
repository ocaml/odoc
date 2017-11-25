.PHONY : build
build :
	jbuilder build --no-buffer --dev

.PHONY : test
test :
	jbuilder runtest --no-buffer --dev

.PHONY : clean
clean :
	jbuilder clean
