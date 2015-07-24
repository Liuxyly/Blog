

for node in 1 2 3 ;do
	echo "========192.168.56.12$node========"
	ssh 192.168.56.12$node $1
done
