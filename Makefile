subdirs := pipe-and-filter

.PHONY: all $(subdirs)

all: $(subdirs)

test: all

$(subdirs):
	$(MAKE) -C $@
