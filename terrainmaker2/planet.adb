package body Planet is
	procedure Init(self: in out Class)
	is
	begin
		self.bounding_radius := self.nominal_radius + self.atmospheric_depth;
	end;

	function Set_Location(self: access Class; location: vec3_t)
		return access Object
	is
	begin
		self.location := location;
		return self;
	end;

	function Set_Radius(self: access Class; radius: number)
		return access Object
	is
	begin
		self.nominal_radius := radius;
		return self;
	end;
end;

