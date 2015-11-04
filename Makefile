subdirs := pipe-and-filter

.PHONY: all clean $(subdirs)

all: $(subdirs)

test: all

clean:
	$(MAKE) clean -C $(subdirs)

$(subdirs):
	$(MAKE) -C $@
