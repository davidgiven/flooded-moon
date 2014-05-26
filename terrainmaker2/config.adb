with Gnat.Command_Line;
with Gnat.Strings;
with Gnat.OS_Lib;
with Ada.Text_IO;
with Utils;

use Gnat.Command_Line;
use Gnat.Strings;
use Ada.Text_IO;
use Utils;

package body Config is
	output_filename_option: aliased String_Access;
	width_option: aliased integer;
	height_option: aliased integer;
	scene_filename_option: aliased String_Access;

	package body Options is
		function Output_Filename return string is
			(output_filename_option.all);
		function Width return integer is
			(width_option);
		function Height return integer is
			(height_option);
		function Scene_Filename return string is
			(scene_filename_option.all);
	end;

	cmdline: Command_Line_Configuration;

	procedure ParseOptions is
	begin
		Define_Switch(cmdline, output_filename_option'access, "-o:",
			Long_Switch => "--output=",
			Help => "Set output image filename");

		Define_Switch(cmdline, width_option'access, "-W:",
			Long_Switch => "--width=",
			Help => "Set width of output file");

		Define_Switch(cmdline, height_option'access, "-H:",
			Long_Switch => "--height=",
			Help => "Set height of output file");

		Define_Switch(cmdline, scene_filename_option'access, "-s:",
			Long_Switch => "--scene=",
			Help => "Supply scene description");

		Getopt(cmdline);

		if (Gnat.Command_Line.Get_Argument /= "") then
			Error("mysterious argument supplied (you probably meant --scene)");
		end if;
		if (Options.Output_Filename'length = 0) then
			Error("must specify an output filename");
		end if;
		if (Options.Width = 0) or (Options.Height = 0) then
			Error("output image size must be non zero (in both directions)");
		end if;
		if (Options.Scene_Filename'length = 0) then
			Error("must specify a scene description");
		end if;
	exception
		when Exit_From_Command_Line =>
			Gnat.OS_Lib.OS_Exit(0);

		when Invalid_Switch =>
			Error("invalid switch '" & Full_Switch & "'");
		when Invalid_Parameter =>
			Error("invalid parameter '" & Full_Switch & "'");
	end;

	procedure Finalize(ws: in out WrappedString) is
	begin
		if (ws.c /= Null_Ptr) then
			Free(ws.c);
		end if;
	end;

	procedure WrapString(ws: in out WrappedString; s: string) is
	begin
		ws.c := New_String(s);
	end;

end;

