unit fhelp;

{$mode objfpc}{$H+}
{$codepage UTF8}   // 字符串常量和字符串字面量需要这个选项

interface

uses
  Classes, SysUtils, Forms, StdCtrls, Types, LCLType, LCLTranslator;

type

  { TformHelp }

  TformHelp = class(TForm)
    memoContent : TMemo;
    bttnClose   : TButton;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);

    procedure bttnCloseClick(Sender: TObject);
    procedure memoContentKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure memoContentMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
  public
  end;

var
  formHelp: TformHelp;

resourcestring
  Res_HelpTitle = 'Help';

  Res_Help = #10 +
    'TomiNote Help'#10 +
    #10 +
    #10 +
    #10 +
    '[Summary]'#10 +
    #10 +
    'The database engine used by this program is SQLite3, you need to install SQLite3 in order to use this program normally, or you can put the SQLite3 dynamic link library file (sqlite3.dll in Windows or libsqlite3.so in Linux) into the "lib" directory of the program''s directory.'#10 +
    #10 +
    'All configuration information for this program is saved in the ".ini" file with the same name as the program, you can delete the configuration file to make the program restore the default settings, configuration file and program file are in the same directory.'#10 +
    #10 +
    'The size of all windows in this program are adjustable, the font name and font size of all windows are also adjustable, if you are not satisfied with the default interface, you can manually adjust them, the adjusted state will be saved in the configuration file.'#10 +
    #10 +
    'This program uses dynamic load node, when you open the database, it will not load all the nodes, only the first depth node is loaded, when you expand a node, it will load its next node, so you don''t have to worry about loading too much data too slow, but the count of nodes should not exceed 2147483640, which is determined by the type of integer used by the program, you can modify the source code to make it support more The number of nodes, but also consume more database space.'#10 +
    #10 +
    'You can use the command line parameter --c or --config to specify the directory of the configuration file, such as: TomiNote --config=~/.config/TomiNote'#10 +
    #10 +
    'You can use the command line parameter --l or --lang to specify the directory of the language files, such as: TomiNote --lang=/usr/share/TomiNote/languages'#10 +
    #10 +
    #10 +
    #10 +
    '[Move Node]'#10 +
    #10 +
    'Move To Target Front:'#10 +
    'Clrl + Shift + Drag -> Release Mouse Button -> Release Ctrl Key'#10 +
    #10 +
    'Move To Target Behind:'#10 +
    'Ctrl + Drag -> Release Mouse Button -> Release Ctrl Key'#10 +
    #10 +
    'Move To Target First Child:'#10 +
    'Clrl + Shift + Drag -> Release Ctrl Key -> Release Mouse Button'#10 +
    #10 +
    'Move To Target Last Child:'#10 +
    'Clrl + Drag -> Release Ctrl Key -> Release Mouse Button'#10 +
    #10 +
    #10 +
    #10 +
    '[Copy Node]'#10 +
    #10 +
    'Copy To Target Front:'#10 +
    'Alt + Shift + Drag -> Release Mouse Button -> Release Alt Key'#10 +
    #10 +
    'Copy To Target Behind:'#10 +
    'Alt + Drag -> Release Mouse Button -> Release Alt Key'#10 +
    #10 +
    'Copy To Target First Child:'#10 +
    'Alt + Shift + Drag -> Release Alt Key -> Release Mouse Button'#10 +
    #10 +
    'Copy To Target Last Child:'#10 +
    'Alt + Drag -> Release Alt Key -> Release Mouse Button'#10 +
    #10 +
    #10 +
    #10 +
    '[Move Node In Siblings]'#10 +
    #10 +
    'Move To Target Front:'#10 +
    'Ctrl + Drag(Move Forward) -> Release Mouse Button -> Release Ctrl Key'#10 +
    #10 +
    'Move To Target Behind:'#10 +
    'Ctrl + Drag(Move Backward) -> Release Mouse Button -> Release Ctrl Key'#10 +
    #10 +
    'You can use Move Up and Move Down functions to move the selected node, if you move up the first node, the node will be moved to the end, if you move down the last node, the node will be moved to the head.'#10 +
    #10 +
    #10 +
    #10 +
    '[Copy Node In Siblings]'#10 +
    #10 +
    'Copy To Target Front:'#10 +
    'Alt + Drag(Move Forward) -> Release Mouse Button -> Release Alt Key'#10 +
    #10 +
    'Copy To Target Behind:'#10 +
    'Alt + Drag(Move Backward) -> Release Mouse Button -> Release Alt Key'#10 +
    #10 +
    #10 +
    #10 +
    '[Other Moves]'#10 +
    #10 +
    'You can move left the node or move right the node, then the node itself looks like it''s moving left or right.'#10 +
    #10 +
    'You can Drag node from the Tree and Drop into the Recycler, or Drag from the Recycler and Drop into the Tree.'#10 +
    #10 +
    #10 +
    #10 +
    '[Delete Node]'#10 +
    #10 +
    'This program supports recycle bin, you can move the node to the recycle bin or restore the node from the recycle bin. You can also delete the node directly without using the recycle bin, when you delete a node directly, there will be a warning, if you want to skip the warning, you can hold the Shift key while deleting.'#10 +
    #10 +
    #10 +
    #10 +
    '[Expand and Collapse]'#10 +
    #10 +
    'When you collapse the node, if you hold down the Shift key, you can collapse to the root node.'#10 +
    #10 +
    #10 +
    #10 +
    '[Recent Files]'#10 +
    #10 +
    'In the "File" menu will list the recent files. The menu order in this list will not change, to facilitate you to quickly find the last opened file. If you open more than 10 files, the oldest opened file item will be removed from the list. If you want to remove one recent file item, you can hold down the Shift key and then click on the recent file item.'#10 +
    #10 +
    'All drop-down lists in this program also support the Shift key to delete item.'#10 +
    #10 +
    #10 +
    #10 +
    '[Auto Save]'#10 +
    #10 +
    'The "Auto Save" Timer will start after opening the file, and will reset after saving the file.'#10 +
    #10 +
    'You can change the "Auto Save" Timer interval in the Options dialog box. If set to 0, "Auto Save" is disabled.'#10 +
    #10 +
    'You can check the "Auto Save" Timer remaining time in the Options dialog box and you can modify it.'#10 +
    #10 +
    'The "Auto Save" function checks the remaining time every minute, and if the remaining time is 0, "Auto Save" is performed and the remaining time is reset.'#10 +
    #10 +
    #10 +
    #10 +
    '[Auto Backup]'#10 +
    #10 +
    'The "Auto Backup" Timer will start after the first time save the file. If you never saved the file after last backup, the "Auto Backup" Timer will not start.'#10 +
    #10 +
    'You can change the "Auto Backup" Timer interval in the Options dialog box. If set to 0, "Auto Backup" is disabled.'#10 +
    #10 +
    'You can change the maximum count of "Auto Backup" files in the Options dialog box. If set to 0, "Auto Backup" is disabled.'#10 +
    #10 +
    'You can check the "Auto Backup" Timer remaining time in the Options dialog box and you can modify it. The remaining time for "Auto Backup" is stored in the database.'#10 +
    #10 +
    'The "Auto Backup" function checks the remaining time every minute, and if the remaining time is 0, "Auto Backup" is performed and the remaining time is reset.'#10 +
    #10 +
    #10 +
    #10 +
    '[History Record]'#10 +
    #10 +
    'History only supports Ctrl + V paste, or paste through the menu, do not support other paste, or history will be wrong.'#10 +
    #10 +
    'You can keep the history of the current node, or you can keep the history of all nodes, you can switch it in the Options dialog box.'#10 +
    #10 +
    'You can limit the maximum capacity and minimum count of single node''s history in the Options dialog box. If you set it to 0, there is no limit.'#10 +
    #10 +
    'You can discard the history to free up memory, which you can do in the Options dialog box.'#10 +
    #10 +
    #10 +
    #10 +
    '[Search And Replace]'#10 +
    #10 +
    'You can search for text in node name and node content. You can search for text in "Selected node" or "Selected node and its descendants" or "All nodes".'#10 +
    #10 +
    'Carriage returns and newline characters in search box and replacement box will be converted to \r \n, tabs will be converted to \t, the backslash will not be processed, only the special backslash will be converted to \\. For example: ''\a\\b''#10 will be converted to ''\a\\b\r'', ''\a\\b\''#10 will be converted to ''\a\\b\\\r''.'#10 +
    #10 +
    'If you perform a search or replace operation, \r \n \t \\ \xFF in the search string or replace string will be converted to carriage returns, newlines, tabs, backslashes, and Ascii characters Char($FF). If two characters after \x are not hexadecimal characters, \x will not be converted.'#10 +
    #10 +
    'The search results will be listed in the InfoBar, which you can double-click to jump to the corresponding place of the search result.'#10 +
    #10 +
    'You can also replace text, replacement results will be listed in the InfoBar, you can use regular expressions in the replace operation.'#10 +
    #10 +
    'After you enter "search content" or "replace content", you can use Ctrl + Enter to perform Search or Replace operation.'#10 +
    #10 +
    'The count limit of search results is 50000, because too many search results can cause the program to become unresponsive. You can change this value in the Options dialog box. If set to 0, there is no limit.'#10 +
    #10 +
    'Note: Regular expressions support only characters within the range of Unicode UCS-2.'#10 +
    #10 +
    'Note: The nodes that executed the multi-node replace operation will lose their history.'#10 +
    #10 +
    'Note: The contents of the search and replace operations are taken from the database, and the line-ending character used in the database is \n, so you can never found the \r character.'#10 +
    #10 +
    'Note: If the pasted content contains line-ending character that do not match the current system, you can convert the line-ending to the current system''s line-ending by "switching to other nodes and then switching back", the wrong line-ending will cause the search results to display incorrectly.'#10 +
    #10 +
    #10 +
    #10 +
    '[Import And Export]'#10 +
    #10 +
    'You can import data from the "text file" or "directory" or "database file" to the current database, you can also export the node or tree in the current database to "text file" or "directory" or "database file".'#10 +
    #10 +
    'The "Save As" operation is implemented using the "Export To Database" function. The "Export To Database" function will compress the fragmented space in the database.'#10 +
    #10 +
    'If you export the data to an existing database file, the original data in the target file will not be overwritten, and the new data will be appended to the end of the target file.'#10 +
    #10 +
    'Note: The files to be imported only supports the text file of the UTF8 format, and the UTF8-BOM at the beginning of the file will be ignored.'#10 +
    #10 +
    'Note: The "Save As" operation will delete the target file first, without retaining the data in the target file.'#10 +
    #10 +
    #10 +
    #10 +
    '[Font Name And Font Size]'#10 +
    #10 +
    'You can set the font name and font size in the Options dialog box, if the font name set is empty, the system default font is used, if the font size is set to 0, the system default font size is used.'#10 +
    #10 +
    'You can also use Ctrl + MouseWheel or Ctrl + Shift + MouseWheel to adjust the font size.'#10 +
    #10 +
    #10 +
    #10 +
    '[Color]'#10 +
    #10 +
    'You can switch between bright theme and dark theme, and you can also customize the foreground and background colors for each theme.'#10 +
    #10 +
    #10 +
    #10 +
    '[Others]'#10 +
    #10 +
    'If you have selected some text before execute "Strip Trailing Space", then only the trailing space in the selected text will be stripped.'#10 +
    #10 +
    'You can check "Auto strip trailing space" in the Options dialog box so that the trailing space are automatically cleared each time you switch nodes, import nodes, or export nodes.'#10 +
    #10 +
    'In the case of the TreeBar is closed, you can use Ctrl + PageUp and Ctrl + PageDown to switch nodes for browsing.'#10 +
    #10 +
    #10 +
    #10 +
    '[Node Utils]'#10 +
    #10 +
    'In the Node Utils, the Sort tool can sort sibling nodes or subnodes, the Split tool can split one note into multiple subnodes, the Rename tool can search for a string in the node name or the note, then replace it with the specified string, and then rename the node using the replacement result. the Script tool can perform multi-replace on multiple nodes.'#10 +
    #10 +
    'In the Split tool, the "Separator" is the symbol that used to split the note, it use the regular expression syntax, the program will search for the sparator in the note, then split the text before and after the separator into two parts, then save them to the different child nodes. the "Title" is also use the regular expression syntax, the program will search the Title string in the splitted text, then the string will be used as the name of the child node. If the "Include Separator" option is checked, the separator string will be include in the splitted text, otherwise it will not be include. If the "Add Prefix Number" option is checked, a serial number will be added to the beginning of the subnode name, the length of the number is specified in the edit box which follow the option. If the "Add Suffix Number" option is checked, a serial numbers will be added to the ending of the subnode name, the length of the number is specified in the edit box which follow the option.'#10 +
    #10 +
    'In the Rename tool, the search string and replace string must use regular expressions. The rename operation will be applied to the current node and its descendant nodes, if a node''s search result is empty, it will be ignored, it will not be renamed. if you checked the "Acts on the nodes at the specified depth" option, only nodes at the specified depth are renamed. you can use ${Num=3,005+5} in the replace string to add a sequence number in the node name. "3," means the rename operation will start at the third node searched, "005" means the first sequence number is 5, the length of the sequence number is 3, "+5" means the step of the sequence number is 5. You can also simply use ${Num=3,} or ${Num=005+5} or ${Num=005}, for example:'#10 +
    #10 +
    'The search string: (Chapter) (.+?)$'#10 +
    'The replace string: $1 ${num=03+2} - $2'#10 +
    #10 +
    'After the rename operation, the content "Chapter I was born" in the first node will be extracted and replaced as "Chapter 03 - I was born", and then used to rename the first node. and the content "Chapter I''m growing up" in the second node will be extracted and replaced as "Chapter 05 - I''m growing up", and then used to rename the second node, and so on.'#10 +
    #10 +
    'The use of Script tool is the same as the Script tool in the Text Utils, but here can be applied to multiple nodes.'#10 +
    #10 +
    #10 +
    #10 +
    '[Text Utils]'#10 +
    #10 +
    'In the Text Utils, there is a Script tool that can be used to replace the text for many consecutive times.'#10 +
    #10 +
    'In the Script tool, the left edit box can be used to enter the name of the script, after input, click the "Add" button to add a script, and then you can enter the script content in the right edit box, the content of the script is very simple, is the duplication of the content of two lines, one line is the search for content, one line is the replacement content, the search for content need "srch=" at the beginning, the replacement content need "repl=" at the beginning, such two lines can be repeated multiple times, for example:'#10 +
    #10 +
    'srch=Dad'#10 +
    'repl=Son'#10 +
    #10 +
    'srch=Mom'#10 +
    'repl=Daughter'#10 +
    #10 +
    'This script will replace all "Dad" with "Son", and replace all "Mom" with "Daughter".'#10 +
    #10 +
    'You can use "case=True" to specify CaseSensitive or "case=False" to specify CaseInSensitive, for example:'#10 +
    #10 +
    'case=true'#10 +
    'srch=Dad'#10 +
    'repl=Son'#10 +
    #10 +
    'case=false'#10 +
    'srch=Mom'#10 +
    'repl=Daughter'#10 +
    #10 +
    'case=True is the default setting.'#10 +
    #10 +
    'In addition, you can use "mode=" to specify the search rule, there are 3 rules to chiose:'#10 +
    #10 +
    'mode=Normal'#10 +
    'mode=RegExpr'#10 +
    'mode=OneOnOne'#10 +
    #10 +
    'mode=Normal is the default setting.'#10 +
    #10 +
    'The "RegExpr" mode is used like this:'#10 +
    #10 +
    'mode=regexpr'#10 +
    'srch=([a-zA-Z])([0-9])'#10 +
    'repl=$1 $2'#10 +
    #10 +
    'srch=([0-9])([a-zA-Z])'#10 +
    'repl=$1 $2'#10 +
    #10 +
    'This script will add a space between all letters and numbers.'#10 +
    #10 +
    'The "OneOnOne" mode is used like this:'#10 +
    #10 +
    'mode=OneOnOne'#10 +
    'case=False'#10 +
    'srch=Dad|Mom|Cat|Chicken'#10 +
    'repl=Son|Daughter|Dog|Duck'#10 +
    #10 +
    'This script will replace "Dad" with "Son", replace "Mom" with "Daughter", replace "Cat" with "Dog", replace "Chicken" with "Duck". This rule can''t use regular expression. If you want to search for "|", you can use \x7C instead of it.'#10 +
    #10 +
    'In addition, you can use "loop=" to specify the number of loop for some replacement operation, for example:'#10 +
    #10 +
    'loop=99999'#10 +
    'mode=regexpr'#10 +
    'srch=“([^“”]*)“'#10 +
    'repl=“$1”'#10 +
    #10 +
    'srch=”([^“”]*)”'#10 +
    'repl=”$1“'#10 +
    'loop=1'#10 +
    #10 +
    'This script will execute replace operation between "loop=99999" and "loop=1" many times, then the content “Quote 1”,“Quote 1“,”Quote 2“,”Quote 3“ will be replaced as “Quote 1”,“Quote 2”,“Quote 3”,“Quote 4”. This will not loop 99999 times, if no matches are found in a loop, the loop will be aborted. If there is no scripts after Loop = 1, Loop = 1 can be omitted. you can also use "loop=" like this (there are 2 loops):'#10 +
    #10 +
    'loop=99999'#10 +
    'mode=regexpr'#10 +
    'srch=“([^“”]*)“'#10 +
    'repl=“$1”'#10 +
    #10 +
    'srch=”([^“”]*)”'#10 +
    'repl=”$1“'#10 +
    #10 +
    'loop=99999'#10 +
    'srch=‘([^‘’]*)‘'#10 +
    'repl=‘$1’'#10 +
    #10 +
    'srch=’([^‘’]*)’'#10 +
    'repl=’$1‘'#10 +
    'loop=1'#10 +
    #10 +
    'Every time a "loop=" is encountered, the previous loop will be executed.'#10 +
    #10 +
    'After you input the script, you can click the "Execute" button to execute the script to replace the current note.'#10 +
    #10 +
    'The "Add" button is followed by the "Delete" button, the "Delete" button is followed by the "Modify" button, the "Modify" button can be used to modify the script name.'#10 +
    #10 +
    #10 +
    #10 +
    '[Partial Shortcuts]'#10 +
    #10 +
    'Search                        : Ctrl + F'#10 +
    'Prev Search Result            : F3'#10 +
    'Next Search Result            : F4'#10 +
    'Node Utils                    : Ctrl + D'#10 +
    'Text Utils                    : Ctrl + G'#10 +
    #10 +
    'Recycle node                  : Ctrl + Delete'#10 +
    'Delete node                   : Alt + Delete'#10 +
    'Delete node (without warning) : Alt + Shift + Delete'#10 +
    #10 +
    'Select the previous node      : Ctrl + PageUp'#10 +
    'Select the next node          : Ctrl + PageDown'#10 +
    #10 +
    'Undo                          : Ctrl + Z'#10 +
    'Redo                          : Ctrl + Shift + Z / Ctrl + Y'#10 +
    #10 +
    'Toggle MenuBar                : F5'#10 +
    'Toggle ToolBar                : F6'#10 +
    'Toggle StatBar                : F7'#10 +
    'Toggle TreeBar                : F9  /  Alt + 1'#10 +
    'Toggle InfoBar                : F10 /  Alt + 2'#10 +
    'Toggle RecyBar                : F8  /  Alt + 3'#10 +
    #10 +
    'Toggle FullScreen             : F11'#10 +
    'Toggle FullWindow             : F12'#10 +
    #10 +
    'Bright Theme                  : Alt + 9'#10 +
    'Dark Theme                    : Alt + 0'#10 +
    'Toggle Theme                  : Alt + 4'#10 +
    #10 +
    #10 +
    #10 +
    '[Language Files]'#10 +
    #10 +
    'You can put your own language file in the "languages" directory to localize the program. The language corresponding to each language file is as follows:'#10 +
    #10 +
    'TomiNote.af.po    = Afrikaans'#10 +
    'TomiNote.am.po    = Amharic'#10 +
    'TomiNote.ar.po    = Arabic'#10 +
    'TomiNote.ar_ae.po = Arabic(United Arab Emirates)'#10 +
    'TomiNote.ar_bh.po = Arabic(Bahrain)'#10 +
    'TomiNote.ar_dz.po = Arabic(Algeria)'#10 +
    'TomiNote.ar_eg.po = Arabic(Egypt)'#10 +
    'TomiNote.ar_iq.po = Arabic(Iraq)'#10 +
    'TomiNote.ar_jo.po = Arabic(Jordan)'#10 +
    'TomiNote.ar_kw.po = Arabic(Kuwait)'#10 +
    'TomiNote.ar_lb.po = Arabic(Lebanon)'#10 +
    'TomiNote.ar_ly.po = Arabic(Libya)'#10 +
    'TomiNote.ar_ma.po = Arabic(Morocco)'#10 +
    'TomiNote.ar_om.po = Arabic(Oman)'#10 +
    'TomiNote.ar_qa.po = Arabic(Qatar)'#10 +
    'TomiNote.ar_sa.po = Arabic(Saudi Arabia)'#10 +
    'TomiNote.ar_sy.po = Arabic(Syria)'#10 +
    'TomiNote.ar_tn.po = Arabic(Tunisia)'#10 +
    'TomiNote.ar_ye.po = Arabic(Yemen)'#10 +
    'TomiNote.as.po    = Assamese'#10 +
    'TomiNote.az.po    = Azeri'#10 +
    'TomiNote.az_az.po = Azeri(Cyrillic)'#10 +
    'TomiNote.be.po    = Belarusian'#10 +
    'TomiNote.bg.po    = Bulgarian'#10 +
    'TomiNote.bn.po    = Bengali'#10 +
    'TomiNote.bo.po    = Tibetan'#10 +
    'TomiNote.bs.po    = Bosnian'#10 +
    'TomiNote.ca.po    = Catalan'#10 +
    'TomiNote.cs.po    = Czech'#10 +
    'TomiNote.cy.po    = Welsh'#10 +
    'TomiNote.da.po    = Danish'#10 +
    'TomiNote.de.po    = German'#10 +
    'TomiNote.de_at.po = German(Austria)'#10 +
    'TomiNote.de_ch.po = German(Switzerland)'#10 +
    'TomiNote.de_de.po = German(Germany)'#10 +
    'TomiNote.de_li.po = German(Liechtenstein)'#10 +
    'TomiNote.de_lu.po = German(Luxembourg)'#10 +
    'TomiNote.dv.po    = Maldivian'#10 +
    'TomiNote.el.po    = Greek'#10 +
    'TomiNote.en.po    = English'#10 +
    'TomiNote.en_au.po = English(Australia)'#10 +
    'TomiNote.en_bz.po = English(Belize)'#10 +
    'TomiNote.en_ca.po = English(Canada)'#10 +
    'TomiNote.en_cb.po = English(Caribbean)'#10 +
    'TomiNote.en_gb.po = English(Great Britain)'#10 +
    'TomiNote.en_ie.po = English(Ireland)'#10 +
    'TomiNote.en_in.po = English(India)'#10 +
    'TomiNote.en_jm.po = English(Jamaica)'#10 +
    'TomiNote.en_nz.po = English(New Zealand)'#10 +
    'TomiNote.en_ph.po = English(Philippines)'#10 +
    'TomiNote.en_tt.po = English(Trinidad)'#10 +
    'TomiNote.en_us.po = English(United States)'#10 +
    'TomiNote.en_za.po = English(Southern Africa)'#10 +
    'TomiNote.es.po    = Spanish'#10 +
    'TomiNote.es_ar.po = Spanish(Argentina)'#10 +
    'TomiNote.es_bo.po = Spanish(Bolivia)'#10 +
    'TomiNote.es_cl.po = Spanish(Chile)'#10 +
    'TomiNote.es_co.po = Spanish(Colombia)'#10 +
    'TomiNote.es_cr.po = Spanish(Costa Rica)'#10 +
    'TomiNote.es_do.po = Spanish(Dominican Republic)'#10 +
    'TomiNote.es_ec.po = Spanish(Ecuador)'#10 +
    'TomiNote.es_es.po = Spanish(Traditional)'#10 +
    'TomiNote.es_gt.po = Spanish(Guatemala)'#10 +
    'TomiNote.es_hn.po = Spanish(Honduras)'#10 +
    'TomiNote.es_mx.po = Spanish(Mexico)'#10 +
    'TomiNote.es_ni.po = Spanish(Nicaragua)'#10 +
    'TomiNote.es_pa.po = Spanish(Panama)'#10 +
    'TomiNote.es_pe.po = Spanish(Peru)'#10 +
    'TomiNote.es_pr.po = Spanish(Puerto Rico)'#10 +
    'TomiNote.es_py.po = Spanish(Paraguay)'#10 +
    'TomiNote.es_sv.po = Spanish(ElSalvador)'#10 +
    'TomiNote.es_uy.po = Spanish(Uruguay)'#10 +
    'TomiNote.es_ve.po = Spanish(Venezuela)'#10 +
    'TomiNote.et.po    = Estonian'#10 +
    'TomiNote.eu.po    = Basque'#10 +
    'TomiNote.fa.po    = Farsi'#10 +
    'TomiNote.fi.po    = Finnish'#10 +
    'TomiNote.fo.po    = Faroese'#10 +
    'TomiNote.fr.po    = French'#10 +
    'TomiNote.fr_be.po = French(Belgium)'#10 +
    'TomiNote.fr_ca.po = French(Canada)'#10 +
    'TomiNote.fr_ch.po = French(Switzerland)'#10 +
    'TomiNote.fr_fr.po = French(France)'#10 +
    'TomiNote.fr_lu.po = French(Luxembourg)'#10 +
    'TomiNote.ga.po    = Irish'#10 +
    'TomiNote.gd.po    = Gaelic(Scotland)'#10 +
    'TomiNote.gd_ie.po = Gaelic(Ireland)'#10 +
    'TomiNote.gl.po    = Galician'#10 +
    'TomiNote.gn.po    = Guarani(Paraguay)'#10 +
    'TomiNote.gu.po    = Gujarati'#10 +
    'TomiNote.he.po    = Hebrew'#10 +
    'TomiNote.hi.po    = Hindi'#10 +
    'TomiNote.hr.po    = Croatian'#10 +
    'TomiNote.hu.po    = Hungarian'#10 +
    'TomiNote.hy.po    = Armenian'#10 +
    'TomiNote.id.po    = Indonesian'#10 +
    'TomiNote.is.po    = Icelandic'#10 +
    'TomiNote.it.po    = Italian'#10 +
    'TomiNote.it_ch.po = Italian(Switzerland)'#10 +
    'TomiNote.it_it.po = Italian(Italy)'#10 +
    'TomiNote.ja.po    = Japanese'#10 +
    'TomiNote.ka.po    = Georgian'#10 +
    'TomiNote.kk.po    = Kazakh'#10 +
    'TomiNote.km.po    = Khmer'#10 +
    'TomiNote.kn.po    = Kannada'#10 +
    'TomiNote.ko.po    = Korean'#10 +
    'TomiNote.ks.po    = Kashmiri'#10 +
    'TomiNote.la.po    = Latin'#10 +
    'TomiNote.lo.po    = Lao'#10 +
    'TomiNote.lt.po    = Lithuanian'#10 +
    'TomiNote.lv.po    = Latvian'#10 +
    'TomiNote.mi.po    = Maori'#10 +
    'TomiNote.mk.po    = FYRO Macedonia'#10 +
    'TomiNote.ml.po    = Malayalam'#10 +
    'TomiNote.mn.po    = Mongolian'#10 +
    'TomiNote.mr.po    = Marathi'#10 +
    'TomiNote.ms.po    = Malay'#10 +
    'TomiNote.ms_bn.po = Malay(Brunei)'#10 +
    'TomiNote.ms_my.po = Malay(Malaysia)'#10 +
    'TomiNote.mt.po    = Maltese'#10 +
    'TomiNote.my.po    = Burmese'#10 +
    'TomiNote.nb.po    = Norwegian(Bokml)'#10 +
    'TomiNote.ne.po    = Nepali'#10 +
    'TomiNote.nl.po    = Dutch'#10 +
    'TomiNote.nl_be.po = Dutch(Belgium)'#10 +
    'TomiNote.nl_nl.po = Dutch(Netherlands)'#10 +
    'TomiNote.no.po    = Norwegian'#10 +
    'TomiNote.or.po    = Oriya'#10 +
    'TomiNote.pa.po    = Punjabi'#10 +
    'TomiNote.pl.po    = Polish'#10 +
    'TomiNote.pt.po    = Portuguese'#10 +
    'TomiNote.pt_br.po = Portuguese(Brazil)'#10 +
    'TomiNote.pt_pt.po = Portuguese(Portugal)'#10 +
    'TomiNote.rm.po    = Raeto(Romance)'#10 +
    'TomiNote.ro.po    = Romanian'#10 +
    'TomiNote.ro_mo.po = Romanian(Moldova)'#10 +
    'TomiNote.ru.po    = Russian'#10 +
    'TomiNote.ru_mo.po = Russian(Moldova)'#10 +
    'TomiNote.sa.po    = Sanskrit'#10 +
    'TomiNote.sb.po    = Sorbian'#10 +
    'TomiNote.sd.po    = Sindhi'#10 +
    'TomiNote.si.po    = Sinhalese'#10 +
    'TomiNote.sk.po    = Slovak'#10 +
    'TomiNote.sl.po    = Slovenian'#10 +
    'TomiNote.so.po    = Somali'#10 +
    'TomiNote.sq.po    = Albanian'#10 +
    'TomiNote.sr.po    = Serbian(Latin)'#10 +
    'TomiNote.sr_sp.po = Serbian(Cyrillic)'#10 +
    'TomiNote.sv.po    = Swedish'#10 +
    'TomiNote.sv_fi.po = Swedish(Finland)'#10 +
    'TomiNote.sv_se.po = Swedish(Sweden)'#10 +
    'TomiNote.sw.po    = Swahili'#10 +
    'TomiNote.sz.po    = Sami(lappish)'#10 +
    'TomiNote.ta.po    = Tamil'#10 +
    'TomiNote.te.po    = Telugu'#10 +
    'TomiNote.tg.po    = Tajik'#10 +
    'TomiNote.th.po    = Thai'#10 +
    'TomiNote.tk.po    = Turkmen'#10 +
    'TomiNote.tn.po    = Setswana'#10 +
    'TomiNote.tr.po    = Turkish'#10 +
    'TomiNote.ts.po    = Tsonga'#10 +
    'TomiNote.tt.po    = Tatar'#10 +
    'TomiNote.uk.po    = Ukrainian'#10 +
    'TomiNote.ur.po    = Urdu'#10 +
    'TomiNote.uz.po    = Uzbek(Latin)'#10 +
    'TomiNote.uz_uz.po = Uzbek(Cyrillic)'#10 +
    'TomiNote.ve.po    = Venda'#10 +
    'TomiNote.vi.po    = Vietnamese'#10 +
    'TomiNote.xh.po    = Xhosa'#10 +
    'TomiNote.yi.po    = Yiddish'#10 +
    'TomiNote.zh.po    = Chinese'#10 +
    'TomiNote.zh_cn.po = Chinese(China)'#10 +
    'TomiNote.zh_hk.po = Chinese(Hong Kong SAR)'#10 +
    'TomiNote.zh_mo.po = Chinese(Macau SAR)'#10 +
    'TomiNote.zh_sg.po = Chinese(Singapore)'#10 +
    'TomiNote.zh_tw.po = Chinese(Taiwan)'#10 +
    'TomiNote.zu.po    = Zulu'#10 +
    #10 +
    #10 +
    #10;

  Res_RegExprHelp = #10'Regular expression syntax help (Translators organize their own content)'#10;

  Res_AboutTitle = 'About';

  Res_About = 'TomiNote is a simple note collection tool.'#10 +
    #10 +
    'License: This program has no license, it is free for everyone. If you use the source code of this program, you only need to pay attention to the license of Lazarus and the license of the icon files.'#10 +
    #10 +
    'The preparation of this program is purely personal hobby, this program does not provide any guarantee, I am not responsible for the loss caused by the use of the program.'#10 +
    #10 +
    'You can get the source code of this program here:'#10 +
    'https://github.com/tomitomy/TomiNote'#10 +
    #10 +
    '------------------------------'#10 +
    #10 +
    'This program was written using Lazarus:'#10 +
    'http://www.lazarus-ide.org'#10 +
    #10 +
    'The License of Lazarus is GPL/LGPL. See Lazarus and Free Pascal sources for license details.'#10 +
    #10 +
    'Thanks to Lazarus Team for designing such good programming software and sharing it to everyone! Thanks to the members of Lazarus Forum for their help!'#10 +
    #10 +
    '------------------------------'#10 +
    #10 +
    'The icons used in this program are download from FatCow:'#10 +
    'http://www.fatcow.com/free-icons'#10 +
    #10 +
    'The License of the Icons is "CCBY 3.0":'#10 +
    'https://creativecommons.org/licenses/by/3.0/'#10 +
    #10 +
    'I modified some of the icons to suit the needs of my program.'#10 +
    #10 +
    'Thanks to FatCow for designing so many good icons and sharing them to everyone!'#10 +
    #10 +
    #10 +
    #10;

implementation

uses
  fmain, uconfig;
{$R *.lfm}

{ TformHelp }

procedure TformHelp.FormCreate(Sender: TObject);
begin
  // 初始化窗口状态
  if Screen.Fonts.IndexOf(Config.WindowFontName) <> -1 then
    Font.Name := Config.WindowFontName;
  Font.Size := Config.WindowFontSize;

  // WindowState := wsMaximized;

  // 初始化控件状态
  case Config.ActiveTheme of
    BrightThemeID: begin
      memoContent.Font.Color := Config.BrightFontColor;
      memoContent.Color := Config.BrightBackColor;
    end;
    DarkThemeID: begin
      memoContent.Font.Color := Config.DarkFontColor;
      memoContent.Color := Config.DarkBackColor;
    end;
  end;

  if Screen.Fonts.IndexOf(Config.NoteBarFontName) <> -1 then
    memoContent.Font.Name := Config.NoteBarFontName;
  memoContent.Font.Size := Config.NoteBarFontSize;

  memoContent.Text := Res_Help;

  // 此代码用于简化语言文件
  bttnClose.Caption := formMain.actnClose.Caption;
end;

procedure TformHelp.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  formHelp := nil;
end;

procedure TformHelp.bttnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TformHelp.memoContentKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Close;
end;

procedure TformHelp.memoContentMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  ASize: integer;
begin
  if not (ssCtrl in Shift) then Exit;

  if ssShift in Shift then ASize := 10 else ASize := 1;
  if WheelDelta < 0 then ASize := -ASize;

  ASize := memoContent.Font.Size + ASize;
  if ASize <= 0 then ASize := 1;

  memoContent.Font.Size := ASize;
end;

end.

