
TomiNote is a simple note collection tool that supports the following features:

- Multi-language switch
- Tree directory
- Move left and move right the node
- Drag and drop nodes (need to press Ctrl or Alt key)
- Recycle bin
- Multi-node history
- Import and export (file, folders, database)
- Automatic save and automatic backup
- Multi-node search and replace (support for regular expressions)
- Sort node
- Split the note into multiple sub-nodes (regular expression)
- Multi-replace (regular expression)
- Adjust the user interface (full screen, full window)
- Custom font name and font size
- Custom foreground and background colors
- Dynamic load child nodes (load child nodes when expanding the node)

For more features, please check the software help information.

This program has only been tested in the Linux Mint 18.2 MATE 64-bit and has not been tested on other platforms. If you compile this program on other platforms, you may need to make the appropriate adjustments.

If you compile this program GTK2 version, please apply all the patches here first:
https://bugs.freepascal.org/view.php?id=32583

The version of lazarus I used is 1.8.0RC4. The patch files are in the LazPathcs folder (It's the same patch files as the above link, used in the Linux version of the Lazarus 1.8.0RC4). If you use other versions of Lazarus to compile this program, you may cause errors in the history function, because TEdit and TMemo controls have changed in other versions.



TomiNote 是一个简单的笔记收集工具，它支持以下特性：

- 多语言切换
- 树形目录
- 左移右移节点
- 拖拽节点（需要按下 Ctrl 或 Alt 键）
- 回收站
- 多节点历史记录
- 导入导出（文件、文件夹、数据库）
- 自动保存和自动备份
- 多节点搜索和替换（支持正则表达式）
- 节点排序
- 将节点文本分割为多个子节点（正则表达式）
- 文本的多重替换
- 自由调整用户界面（全屏、满窗）
- 自定义字体名称和字体大小
- 自定义前景色和背景色
- 动态加载子节点（展开节点的时候载入子节点）

更多功能请查看软件的帮助信息。

本程序只在 Linux Mint 18.2 MATE 64-bit 中做了测试，其它平台未做测试，如果你在其它平台编译本程序，可能需要做适当调整。

如果你编译本程序的 GTK2 版本，请先应用这里的所有补丁：
https://bugs.freepascal.org/view.php?id=32583

我使用的 Lazarus 版本是 1.8.0RC4，补丁文件在 LazPatchs 目录中（它与上面链接中的补丁文件相同，用在 Linux 版本的 Lazarus 1.8.0RC4 中），如果使用其它版本的 Lazarus 编译本程序，可能会造成历史记录功能出错，因为 TEdit 和 TMemo 控件在其它版本中有所改变。
