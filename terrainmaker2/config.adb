with Gnat.Command_Line;
with Gnat.Strings;
with Gnat.OS_Lib;
with Ada.Text_IO;

use Gnat.Command_Line;
use Gnat.Strings;
use Ada.Text_IO;

package body Config is
	output_filename_option: aliased String_Access;

	package body Options is
		function Output_Filename return string is
			(output_filename_option.all);
	end;

	procedure cmd_help is
	begin
		Put_Line("help!");
	end;

	cmdline: Command_Line_Configuration;
begin
	Define_Switch(cmdline, output_filename_option'access, "-o:",
		Long_Switch => "--output=",
		Help => "Set output image filename");

	Getopt(cmdline);

	if (Options.Output_Filename'length = 0) then
		Put_Line(Standard_Error, "terrainmaker: must specify an output filename!");
		Gnat.OS_Lib.OS_Exit(1);
	end if;

exception
	when Exit_From_Command_Line =>
		Gnat.OS_Lib.OS_Exit(0);

	when Invalid_Switch =>
		Gnat.OS_Lib.OS_Exit(1);
end;

