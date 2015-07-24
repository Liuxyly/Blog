

for node in 1 2 3;do
	echo "========192.168.56.12$node========"
	ssh 192.168.56.12$node 'for src in `ls /etc/init.d|grep '$1'`;do service $src '$2'; done'
done
