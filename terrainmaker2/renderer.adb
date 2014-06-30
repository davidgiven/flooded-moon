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
	function Render(width, height: integer) return image_t is
 		thread_count: integer := (
			if (Options.Number_Of_Threads = 0)
			then integer(System.Multiprocessors.number_Of_CPUs)
			else Options.Number_Of_Threads
		);

		screen: image_t := Images.Create(width, height);

		task Scheduler is
			-- Each worker calls this to find out what it needs to do.
			entry RequestWorkUnit(y: out integer; finished: out boolean);
		end;
 
		task body Scheduler is
			progress: integer := -1;
		begin
			-- Hand out each scanline in turn to tasks that want things to
			-- do.
			for yy in screen.pixels.data'range(2) loop
				accept RequestWorkUnit(y: out integer; finished: out boolean) do
					y := yy;
					finished := false;
				end RequestWorkUnit;

				-- Update progress bar.
				declare
					scanline: integer := yy - screen.pixels.data'first(2);
					p: integer := integer(scanline)*Progress_Bar_Size /
						screen.pixels.data'length(2);
				begin
					if (p /= progress) then
						progress := p;
						
						Put('[');
						for i in 1..progress loop
							Put('=');
						end loop;
						for i in (progress+1)..(Progress_Bar_Size-1) loop
							Put(' ');
						end loop;
						Put("]");
						Put(CR);
						Flush;
					end if;
				end;
			end loop;

			-- Now tell each worker thread it's done (each thread will ask
			-- once).
			for i in 1..thread_count loop
				accept RequestWorkUnit(y: out integer; finished: out boolean) do
					y := 0;
					finished := true;
				end RequestWorkUnit;
			end loop;

			Put(LF);
		end;
 
		-- Actually does the rendering. Each of these is self contained and will
		-- keep working until there's nothing left to do, at which point it
		-- exits.
		task type Worker;
		task body Worker is
			y: integer;
			finished: boolean;
			r: ray_t;
		begin
			-- Keep asking for stuff to do, then do it. When the Scheduler
			-- has terminated, requesting a work unit will throw an exception and
			-- the task will safely exit.
			loop
				Scheduler.RequestWorkUnit(y, finished);
				exit when finished;

				for x in screen.pixels.data'range(1) loop
					r := Compute_Primary_Ray(x, y, screen);
					screen(x, y) := Compute_Pixel_Colour(r);
				end loop;
			end loop;
		exception
			when e: others =>
				Error("Exception thrown from worker thread!" & LF &
					Ada.Exceptions.Exception_Information(e));
		end;
 
	begin
		Put_Line("Rendering with" & thread_count'img & " threads");

		declare
			-- Create some work threads (which will automatically start).
			scanlines: array(1..thread_count) of Worker;
		begin
			null; -- just wait for threads to exit
		end;

		Put_Line(LF & "done");
		return screen;
	end;
end;


