Caml1999N034����         	   <src/driver/library_names.mli����     p  	�  	U�����1ocaml.ppx.context��&_none_@@ �A����������)tool_name���*ppx_driver@@@����,include_dirs����"[]@@@����3hidden_include_dirs!����
%@%@@����)load_path*�������1@1@@�����6@6@@@6@@����,open_modules;����$?@?@@����+for_packageD����$NoneI@I@@����%debugN����%falseS@S@@����+use_threadsX����
\@\@@����-use_vmthreadsa����e@e@@����/recursive_typesj����n@n@@����)principals����%w@w@@����3transparent_modules|����.�@�@@����-unboxed_types�����7�@�@@����-unsafe_string�����@�@�@@����'cookies�����"::�����������,library-name�@�@@����/odoc_driver_lib��.<command-line>A@A�A@P@@��A@@�A@Q@@@@�@@�������@�@@@�@@�@@@@�@@@�@�����A�  # �'library��<src/driver/library_names.mliA@E�A@L@@@��Р$name��	BQS�
BQW@@����&string��BQZ�BQ`@@��BQZ�BQ`@@@��BQS�BQa@@�Р,archive_name��Cbd�Cbp@@����&option��&Cbz�'Cb @@�����&string��/Cbs�0Cby@@��2Cbs�3Cby@@@@��5Cbs�6Cb @@@@��8Cbd�9Cb A@@�Р#dir��?D B D�@D B G@@����&option��GD B Q�HD B W@�����&string��PD B J�QD B P@@��SD B J�TD B P@@@@��VD B J�WD B W@@@��YD B D�ZD B X@@�Р$deps��`E Y [�aE Y _@@����$list��hE Y i�iE Y m@�����&string��qE Y b�rE Y h@@��tE Y b�uE Y h@@@@��wE Y b�xE Y m@@@��zE Y [�{E Y n@@@A@@��}A@@�~F o p@@���A@@��F o p@���A�  # �!t���H r w��H r x@@@��Р(meta_dir���H r }��H r �@@�����%Fpath!t���H r ���H r �@@���H r ���H r �@@@���H r }��H r �@@�Р)libraries���H r ���H r �@@����$list���H r ���H r �@�����'library���H r ���H r �@@���H r ���H r �@@@@���H r ���H r �@@@���H r ���H r �@@@A@@���H r r��H r �@@���H r r��H r �@���Р1process_meta_file���J � ���J � �@��@�����%Fpath!t���J � ���J � �@@���J � ���J � �@@@����!t���J � ���J � �@@���J � ���J � �@@@���J � ���J � �@@@@���)ocaml.doc���@@ ���@@ �A�������	W From a path to a [Meta] file, returns the list of libraries defined in this
    file. ��K � ��L".@@��K � ��L".@@@@��K � ��L".@@��
K � ��L".@@��J � ��J � �@��J � ��J � �@���Р2libname_of_archive��N04�N0F@��@����!t��#N0I�$N0J@@��&N0I�'N0J@@@�����%Fpath#map��0N0U�1N0^@�����&string��9N0N�:N0T@@��<N0N�=N0T@@@@��?N0N�@N0^@@@��BN0I�CN0^@@@@���S��@@ ��@@ �A�������
   [libname_of_archive meta_dir libraries] computes a map from the fully-qualified
    archive path to the name of the library. [meta_path] is the path of the 
    directory where the META file is found, and [libraries] are the libraries
    defined in that META file. ��SO__�TRNo@@��VO__�WRNo@@@@��YO__�ZRNo@@��\O__�]RNo@@��_N00�`N0^@��bN00�cN0^@���Р+directories��kTqu�lTq�@��@����!t��uTq��vTq�@@��xTq��yTq�@@@�����%Fpath#set���Tq���Tq�@@���Tq���Tq�@@@���Tq���Tq�@@@@������M@@ ��N@@ �A�������	� [directories meta_dir libraries] computes a set of directories
    containing the libraries in [libraries] defined in the META file
    found in [meta_path]. ���U����W6@@���U����W6@@@@���U����W6@@���U����W6@@���Tqq��Tq�@���Tqq��Tq�@@