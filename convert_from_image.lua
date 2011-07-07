local string_sub = string.sub
local math_floor = math.floor

local width = arg[1]
local height = arg[2]

local data = {}
local min = 0
local max = 0

local old = 0
for line in io.stdin:lines() do
    for n in line:gfind("[^ ]+") do
        local v = n + 0
        if (v == 99999) then
            v = old
        end
        if (v < min) then
            min = v
        end
        if (v > max) then
            max = v
        end
        data[#data+1] = v
        old = v
    end
end

if (#data ~= (width * height)) then
    io.stderr:write("Error --- amount of data in input file ("..#data..") "..
        "doesn't match width and height supplied on command line!\n")
    os.exit(1)
end

local range = max - min
io.stderr:write("min="..min.." max="..max.."\n")
io.stderr:write("\n")
io.stderr:write("range="..range.."\n")
io.stderr:write("offset="..(-min).."\n")

print("P2")
print(width.." "..height)
print("65535")
for n = 1, width*height do
	local d = data[n]
	d = math_floor(65535 * ((d - min) / range))
	print(d)
end
