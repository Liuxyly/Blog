#!/usr/bin/env bash

# The output of all these installation steps is noisy. With this utility
# the progress report is nice and concise.
function install {
    echo Installing $1
    shift
    yum -y install "$@" >/dev/null 2>&1
}

echo "Update /etc/hosts"
cat > /etc/hosts <<EOF
127.0.0.1       localhost

192.168.56.121 cdh1
192.168.56.122 cdh2
192.168.56.123 cdh3
EOF

echo "Remove unused logs"
sudo rm -rf /root/anaconda-ks.cfg /root/install.log /root/install.log.syslog /root/install-post.log

echo "Disable iptables"
setenforce 0 >/dev/null 2>&1 && iptables -F

### Set env ###
echo "export LC_ALL=en_US.UTF-8"  >>  /etc/profile
cp /usr/share/zoneinfo/Asia/Shanghai /etc/localtime

echo "Setup yum repos"
rm -rf /etc/yum.repos.d/*
cp /vagrant/*.repo /etc/yum.repos.d/
yum clean all >/dev/null 2>&1

echo "Setup root account"
# Setup sudo to allow no-password sudo for "admin". Additionally,
# make "admin" an exempt group so that the PATH is inherited.
cp /etc/sudoers /etc/sudoers.orig
echo "root            ALL=(ALL)               NOPASSWD: ALL" >> /etc/sudoers
echo 'redhat'|passwd root --stdin >/dev/null 2>&1

echo "Setup nameservers"
# http://ithelpblog.com/os/linux/redhat/centos-redhat/howto-fix-couldnt-resolve-host-on-centos-redhat-rhel-fedora/
# http://stackoverflow.com/a/850731/1486325
echo "nameserver 8.8.8.8" | tee -a /etc/resolv.conf
echo "nameserver 8.8.4.4" | tee -a /etc/resolv.conf

echo "Setup ssh"
[ ! -d /root/.ssh ] && ( mkdir /root/.ssh ) && ( chmod 600 /root/.ssh  ) && yes|ssh-keygen -f ~/.ssh/id_rsa -t rsa -N ""

install Git git
install "Base tools" vim wget curl
install "Hadoop dependencies" expect rsync pssh

install PostgreSQL postgresql-server postgresql-jdbc
#sudo -u postgres createuser --superuser vagrant
#sudo -u postgres createdb -O vagrant test1
#sudo -u postgres createdb -O vagrant test2


echo 'All set, rock on!'
