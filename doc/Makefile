.PHONY: all debs watch
BUILD_DIR=_build

all:
	sphinx-build . $(BUILD_DIR)

debs:
	sudo apt install python3-sphinx-rtd-theme python3-sphinx-autobuild

watch:
	sphinx-autobuild . $(BUILD_DIR) --port 8008

clean:
	$(RM) -r $(BUILD_DIR)
