with Gnat.Command_Line;
with Gnat.Strings;
with Gnat.OS_Lib;
with Ada.Text_IO;

use Gnat.Command_Line;
use Gnat.Strings;
use Ada.Text_IO;

package body Config is
	output_filename_option: aliased String_Access;
	width_option: aliased integer;
	height_option: aliased integer;

	package body Options is
		function Output_Filename return string is
			(output_filename_option.all);
		function Width return integer is
			(width_option);
		function Height return integer is
			(height_option);
	end;

	procedure error(msg: string) is
	begin
		Put_Line(Standard_Error, "terrainmaker: " & msg);
		Gnat.OS_Lib.OS_Exit(1);
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

	Define_Switch(cmdline, width_option'access, "-W:",
		Long_Switch => "--width=",
		Help => "Set width of output file");

	Define_Switch(cmdline, height_option'access, "-H:",
		Long_Switch => "--height=",
		Help => "Set height of output file");

	Getopt(cmdline);

	if (Options.Output_Filename'length = 0) then
		error("must specify an output filename");
	end if;
	if (Options.Width = 0) or (Options.Height = 0) then
		error("output image size must be non zero (in both directions)");
	end if;

exception
	when Exit_From_Command_Line =>
		Gnat.OS_Lib.OS_Exit(0);

	when Invalid_Switch =>
		error("invalid switch '" & Full_Switch & "'");
	when Invalid_Parameter =>
		error("invalid parameter '" & Full_Switch & "'");
end;

