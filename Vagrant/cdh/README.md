hadoop-install
==============

在 CentOs 机器上快速安装 CDH 集群的 shell 脚本，本脚本目前实现的是在一个管理节点上安装 NameNode、ResourceManager 等组件，其他节点上安装 DataNode、NodeManager 等组件，尚未实现更加细粒度定制每个节点安装的组件。

# Overview

目录结构如下：

```
⇒  tree -L 2
.
├── README.md
└── shell
    ├── bin
    ├── conf
    ├── install.sh
    ├── script
    ├── security
    ├── template
    └── uninstall.sh

6 directories, 3 files
```

结构说明：

- bin ： 存放安装脚本
- conf : 存放配置文件
- install.sh：安装集群的入口文件
- script：管理集群的脚本
- security：安全相关的
- template：模板文件，包括 hadoop 的配置文件、JCE、Postgresql 驱动等
- uninstall.sh：卸载集群脚本

# Install 

运行之前，首先修改 conf 目录下的 namenode 文件 和 datanode 文件，namenode 文件内容填当前脚本所在的节点名称，该节点将作为集群管理节点，安装 NameNode、ResourceManager 、Postgresql、hive metastore 等组件；datanode 内容为集群 DataNode 节点的主机名称，一行一个名称。

运行 install.sh ，开始安装集群。

# License

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at

[http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
