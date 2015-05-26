obj-m+=hello_thread.o
CURRENT:=$(shell pwd)
KERNEL_PATH:=/usr/src/linux-headers-$(shell uname -r)

all:
	make -C $(KERNEL_PATH) M=$(CURRENT) modules
clean:
	make -C $(KERNEL_PATH) M=$(CURRENT) clean
