with Config;

use Config;

package Utils is
	procedure Error(msg: string);

	function Min(n1, n2: number) return number is
		(if (n1 < n2) then (n1) else (n2));
	function Max(n1, n2: number) return number is
		(if (n1 > n2) then (n1) else (n2));
	function Clamp(n, lo, hi: number) return number is
		(Max(Min(n, hi), lo));
end;

