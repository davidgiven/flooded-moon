with Ada.Text_IO;
with Gnat.OS_Lib;

use Ada.Text_IO;

package body Utils is
	procedure Error(msg: string) is
	begin
		Put_Line(Standard_Error, "terrainmaker: " & msg);
		Gnat.OS_Lib.OS_Exit(1);
	end;
end;

