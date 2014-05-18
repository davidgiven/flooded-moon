with Ada.Text_IO;
with Config;
with Images;
with Colours;
with System.Multiprocessors;

use Ada.Text_IO;
use Config;
use Images;
use Colours;

package body Renderer is
	function RenderPixel(x, y: integer) return Colour is
	begin
		return RGB(1.0, 0.0, 0.0);
	end;

	function Render(width, height: integer) return Image is
		screen: Image := Images.Create(width, height);

		task Scheduler is
			-- Each worker calls this to find out what it needs to do.
			entry RequestWorkUnit(y: out integer);
		end;
 
		task body Scheduler is
		begin
			-- Hand out each scanline in turn to tasks that want things to
			-- do, then exit.
			for yy in screen.pixels.data'range(2) loop
				accept RequestWorkUnit(y: out integer) do
					y := yy;
				end RequestWorkUnit;
			end loop;
		end;
 
		-- Actually does the rendering. Each of these is self contained and will
		-- keep working until there's nothing left to do, at which point it
		-- exits.
		task type Worker;
		task body Worker is
			y: integer;
		begin
			-- Keep asking for stuff to do, then do it. When the Scheduler
			-- has terminated, requesting a work unit will throw an exception and
			-- the task will safely exit.
			loop
				Scheduler.RequestWorkUnit(y);

				for x in screen.pixels.data'range(1) loop
					screen(x, y) := RenderPixel(x, y);
				end loop;
			end loop;
		end;
 
		-- Create some work threads (which will automatically start).
		scanlines: array(System.Multiprocessors.CPU_Range
			range 1..System.Multiprocessors.Number_Of_CPUs) of Worker;
  begin
    return screen;
  end;
end;


