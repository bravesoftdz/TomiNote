
TomiNote is a simple note collection tool that supports the following features:

01, support multi-language switch
02, support tree directory
03, support move left and move right the node
04, support drag and drop nodes (need to press Ctrl or Alt key)
05, support recycle bin
06, support multi-node history
07, support import and export (file, folders, database)
08, support automatic save and automatic backup
09, support multi-node search and replace (support for regular expressions)
10, support sort node
11, support split the note into multiple sub-nodes (regular expression)
12, support multi-replace (regular expression)
13, support adjust the user interface (full screen, full window)
14, support custom font name and font size
15, support custom foreground and background colors
16, support dynamic load child nodes (load child nodes when expanding the node)

For more features, please check the software help information.

This program has only been tested in the Linux Mint 18.2 MATE 64-bit and has not been tested on other platforms. If you compile this program on other platforms, you may need to make the appropriate adjustments.

If you compile this program GTK2 version, please apply all the patches here first:
https://bugs.freepascal.org/view.php?id=32583

The version of lazarus I used is 1.8.0RC4. The patch files are in the LazPathcs folder. If you use other versions of Lazarus to compile this program, you may cause errors in the history function, because TEdit and TMemo controls have changed in other versions.



TomiNote 是一个简单的笔记收集工具，它支持以下特性：

01、支持多语言切换
02、支持树形目录
03、支持左移右移节点
04、支持拖拽节点（需要按下 Ctrl 或 Alt 键）
05、支持回收站
06、支持多节点历史记录
07、支持导入导出（文件、文件夹、数据库）
08、支持自动保存和自动备份
09、支持多节点搜索和替换（支持正则表达式）
10、支持节点排序
11、支持将节点文本分割为多个子节点（正则表达式）
12、支持文本的多重替换
13、支持自由调整用户界面（全屏、满窗）
14、支持自定义字体名称和字体大小
15、支持自定义前景色和背景色
16、支持动态加载子节点（展开节点的时候载入子节点）

更多功能请查看软件的帮助信息。

本程序只在 Linux Mint 18.2 MATE 64-bit 中做了测试，其它平台未做测试，如果你在其它平台编译本程序，可能需要做适当调整。

如果你编译本程序的 GTK2 版本，请先应用这里的所有补丁：
https://bugs.freepascal.org/view.php?id=32583

我使用的 Lazarus 版本是 1.8.0RC4，补丁文件在 LazPatchs 目录中，如果使用其它版本的 Lazarus 编译本程序，可能会造成历史记录功能出错，因为 TEdit 和 TMemo 控件在其它版本中有所改变。
