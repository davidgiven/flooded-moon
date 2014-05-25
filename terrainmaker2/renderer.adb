with Ada.Text_IO;
with Ada.Exceptions;
with Config;
with Images;
with Colours;
with Scene;
with System.Multiprocessors;
with Utils;

use Ada.Text_IO;
use Config;
use Images;
use Colours;
use Scene;
use Utils;

package body Renderer is
	function RenderPixel(r: Ray) return Colour is
		ints: Intersections := ComputeObjectIntersections(r);
	begin
		return (if (ints'length>0) then RGB(1.0, 0.0, 0.0) else RGB(0.0, 1.0, 0.0));
	end;

	function Render(width, height: integer) return Image is
		screen: Image := Images.Create(width, height);

		task Scheduler is
			-- Each worker calls this to find out what it needs to do.
			entry RequestWorkUnit(y: out integer; finished: out boolean);
		end;
 
		task body Scheduler is
		begin
			-- Hand out each scanline in turn to tasks that want things to
			-- do.
			for yy in screen.pixels.data'range(2) loop
				accept RequestWorkUnit(y: out integer; finished: out boolean) do
					y := yy;
					finished := false;
				end RequestWorkUnit;
			end loop;

			-- Now tell each worker thread it's done (each thread will ask
			-- once).
			for i in 1..System.Multiprocessors.Number_Of_CPUs loop
				accept RequestWorkUnit(y: out integer; finished: out boolean) do
					finished := true;
				end RequestWorkUnit;
			end loop;
		end;
 
		-- Actually does the rendering. Each of these is self contained and will
		-- keep working until there's nothing left to do, at which point it
		-- exits.
		task type Worker;
		task body Worker is
			y: integer;
			finished: boolean;
			r: Ray;
		begin
			-- Keep asking for stuff to do, then do it. When the Scheduler
			-- has terminated, requesting a work unit will throw an exception and
			-- the task will safely exit.
			loop
				Scheduler.RequestWorkUnit(y, finished);
				exit when finished;

				for x in screen.pixels.data'range(1) loop
					r := ComputePrimaryRay(x, y, screen);
					screen(x, y) := RenderPixel(r);
				end loop;
			end loop;
		exception
			when e: others =>
				Error("Exception thrown from worker thread!" & LF &
					Ada.Exceptions.Exception_Information(e));
		end;
 
	begin
		Put_Line("Rendering with" & System.Multiprocessors.Number_Of_CPUs'img &
			" threads");

		declare
			-- Create some work threads (which will automatically start).
			scanlines: array(System.Multiprocessors.CPU_Range
				range 1..System.Multiprocessors.Number_Of_CPUs) of Worker;
		begin
			null; -- just wait for threads to exit
		end;

		Put_Line("done");
		return screen;
	end;
end;


