obj-m +=hello.o
CURRENT_DIR:=$(shell pwd)
KERNEL_DIR:=$(shell uname -r)
KERNEL_PATH:=/usr/src/linux-headers-$(KERNEL_DIR)

all:
	make -C $(KERNEL_PATH) M=$(CURRENT_DIR) modules
clean:
	make -C $(KERNEL_PATH) M=$(CURRENT_DIR) clean
