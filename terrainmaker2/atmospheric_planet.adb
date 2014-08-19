package body Atmospheric_Planet is
	procedure Init(self: in out Class)
	is
	begin
		Super(self).Init;
	end;

	function Set_Atmospheric_Depth(self: access Class; depth: number)
		return access Object
	is
	begin
		self.atmospheric_depth := depth;
		return self;
	end;
end;


