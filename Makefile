.PHONY : build
build :
	jbuilder build --dev

.PHONY : test
test :
	jbuilder runtest

.PHONY : clean
clean :
	jbuilder clean
