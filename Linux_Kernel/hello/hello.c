#include <linux/init.h>
#include <linux/module.h>
#include <linux/kernel.h>

MODULE_LICENSE("GPL");
extern int hello_data;

static int hello_init(void)
{
	printk(KERN_ERR "hello,kernel!,this is hello module\n");
	printk(KERN_ERR "hello_data:%d\n",++hello_data);
	return 0;
}

static void hello_exit(void)
{
	printk(KERN_ERR "hello_data:%d\n",--hello_data);
	printk(KERN_ERR "Leave hello module!\n");
}
module_init(hello_init);
module_exit(hello_exit);

MODULE_AUTHOR("Liuxy");
MODULE_DESCRIPTION("This is hello module");
MODULE_ALIAS("A simple example");
