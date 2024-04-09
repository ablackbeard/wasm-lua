--[[local wasmFile = {
	0x00, 0x61, 0x73, 0x6D, 0x01, 0x00, 0x00, 0x00, 0x01, 0x14, 0x04, 0x60, 0x00, 0x00, 0x60, 0x00,
	0x01, 0x7F, 0x60, 0x05, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x00, 0x60, 0x00, 0x01, 0x7D, 0x02, 0x4F,
	0x05, 0x03, 0x65, 0x6E, 0x76, 0x07, 0x65, 0x78, 0x74, 0x46, 0x75, 0x6E, 0x63, 0x00, 0x00, 0x03,
	0x65, 0x6E, 0x76, 0x08, 0x65, 0x78, 0x74, 0x46, 0x75, 0x6E, 0x63, 0x32, 0x00, 0x00, 0x03, 0x65,
	0x6E, 0x76, 0x0B, 0x72, 0x65, 0x6E, 0x64, 0x65, 0x72, 0x46, 0x72, 0x61, 0x6D, 0x65, 0x00, 0x00,
	0x03, 0x65, 0x6E, 0x76, 0x09, 0x67, 0x6C, 0x6F, 0x62, 0x61, 0x6C, 0x76, 0x61, 0x72, 0x03, 0x7F,
	0x01, 0x03, 0x65, 0x6E, 0x76, 0x03, 0x6D, 0x65, 0x6D, 0x02, 0x01, 0x80, 0x02, 0x80, 0x02, 0x03,
	0x0A, 0x09, 0x01, 0x01, 0x01, 0x01, 0x01, 0x02, 0x03, 0x01, 0x01, 0x07, 0x58, 0x08, 0x07, 0x74,
	0x65, 0x73, 0x74, 0x66, 0x6E, 0x63, 0x00, 0x03, 0x08, 0x74, 0x65, 0x73, 0x74, 0x66, 0x6E, 0x63,
	0x32, 0x00, 0x04, 0x08, 0x74, 0x65, 0x73, 0x74, 0x66, 0x6E, 0x63, 0x33, 0x00, 0x05, 0x08, 0x74,
	0x65, 0x73, 0x74, 0x66, 0x6E, 0x63, 0x34, 0x00, 0x06, 0x08, 0x74, 0x65, 0x73, 0x74, 0x66, 0x6E,
	0x63, 0x35, 0x00, 0x07, 0x08, 0x74, 0x65, 0x73, 0x74, 0x66, 0x6E, 0x63, 0x36, 0x00, 0x09, 0x08,
	0x74, 0x65, 0x73, 0x74, 0x66, 0x6E, 0x63, 0x37, 0x00, 0x0A, 0x08, 0x74, 0x65, 0x73, 0x74, 0x66,
	0x6E, 0x63, 0x38, 0x00, 0x0B, 0x0A, 0x80, 0x02, 0x09, 0x0F, 0x01, 0x01, 0x7F, 0x41, 0x08, 0x21,
	0x00, 0x20, 0x00, 0x20, 0x00, 0x6A, 0x10, 0x00, 0x0B, 0x0C, 0x00, 0x41, 0x07, 0x41, 0x0A, 0x36,
	0x02, 0x00, 0x41, 0x85, 0x08, 0x0B, 0x10, 0x00, 0x41, 0xA8, 0xB6, 0x2F, 0x41, 0x0A, 0x04, 0x40,
	0x10, 0x00, 0x05, 0x10, 0x01, 0x0B, 0x0B, 0x0E, 0x00, 0x41, 0x01, 0x04, 0x7F, 0x41, 0xFB, 0x00,
	0x05, 0x41, 0xC1, 0x02, 0x0B, 0x0B, 0x15, 0x00, 0x43, 0xC3, 0xF5, 0x48, 0x40, 0x43, 0x00, 0x00,
	0x00, 0x00, 0x5D, 0x04, 0x7F, 0x41, 0x01, 0x05, 0x41, 0x00, 0x0B, 0x0B, 0x3B, 0x01, 0x01, 0x7F,
	0x20, 0x01, 0x41, 0x80, 0x02, 0x6C, 0x20, 0x00, 0x6A, 0x41, 0x03, 0x6C, 0x41, 0x80, 0x80, 0xF4,
	0x07, 0x6A, 0x21, 0x05, 0x20, 0x05, 0x20, 0x02, 0x3A, 0x00, 0x00, 0x20, 0x05, 0x41, 0x01, 0x6A,
	0x21, 0x05, 0x20, 0x05, 0x20, 0x03, 0x3A, 0x00, 0x00, 0x20, 0x05, 0x41, 0x01, 0x6A, 0x21, 0x05,
	0x20, 0x05, 0x20, 0x04, 0x3A, 0x00, 0x00, 0x0B, 0x52, 0x01, 0x02, 0x7F, 0x23, 0x00, 0x41, 0x01,
	0x6A, 0x24, 0x00, 0x03, 0x40, 0x20, 0x00, 0x41, 0x0A, 0x6A, 0x20, 0x01, 0x41, 0x0A, 0x6A, 0x41,
	0xFF, 0x01, 0x41, 0xFF, 0x01, 0x41, 0xFF, 0x01, 0x10, 0x08, 0x20, 0x00, 0x41, 0x01, 0x6A, 0x21,
	0x00, 0x20, 0x00, 0x41, 0xC0, 0x00, 0x4A, 0x04, 0x40, 0x41, 0x00, 0x21, 0x00, 0x20, 0x01, 0x41,
	0x01, 0x6A, 0x21, 0x01, 0x20, 0x01, 0x41, 0x20, 0x48, 0x04, 0x40, 0x0C, 0x02, 0x0B, 0x05, 0x0C,
	0x01, 0x0B, 0x0B, 0x10, 0x02, 0x43, 0xA3, 0x79, 0xEB, 0x4C, 0x0B, 0x10, 0x00, 0x41, 0x01, 0x04,
	0x40, 0x0C, 0x00, 0x10, 0x00, 0x0B, 0x10, 0x01, 0x41, 0xD2, 0x09, 0x0B, 0x0B, 0x01, 0x01, 0x7F,
	0x41, 0xD2, 0xE6, 0x96, 0x02, 0x22, 0x00, 0x0B, 0x0B, 0x0D, 0x01, 0x00, 0x41, 0x10, 0x0B, 0x07,
	0x61, 0x73, 0x64, 0x61, 0x73, 0x64, 0x00
}]]

--local wasmFile = {0x00, 0x61, 0x73, 0x6D, 0x01, 0x00, 0x00, 0x00, 0x01, 0x05, 0x01, 0x60, 0x00, 0x01, 0x7F, 0x03, 0x02, 0x01, 0x00, 0x07, 0x08, 0x01, 0x04, 0x74, 0x65, 0x73, 0x74, 0x00, 0x00, 0x0A, 0x0A, 0x01, 0x08, 0x00, 0x41, 0x14, 0x41, 0xDA, 0x00, 0x6A, 0x0B}

--local wasmFile = {0x00, 0x61, 0x73, 0x6D, 0x01, 0x00, 0x00, 0x00, 0x01, 0x05, 0x01, 0x60, 0x00, 0x01, 0x7B, 0x03, 0x02, 0x01, 0x00, 0x07, 0x08, 0x01, 0x04, 0x74, 0x65, 0x73, 0x74, 0x00, 0x00, 0x0A, 0x16, 0x01, 0x14, 0x00, 0xFD, 0x0C, 0x7B, 0x00, 0x00, 0x00, 0x7B, 0x00, 0x00, 0x00, 0x7B, 0x00, 0x00, 0x00, 0x7B, 0x00, 0x00, 0x00, 0x0B}

--local wasmFile = {0x00, 0x61, 0x73, 0x6D, 0x01, 0x00, 0x00, 0x00, 0x01, 0x05, 0x01, 0x60, 0x00, 0x01, 0x7B, 0x03, 0x02, 0x01, 0x00, 0x05, 0x03, 0x01, 0x00, 0x01, 0x07, 0x08, 0x01, 0x04, 0x74, 0x65, 0x73, 0x74, 0x00, 0x00, 0x0A, 0x0B, 0x01, 0x09, 0x00, 0x41, 0xE3, 0x00, 0xFD, 0x00, 0x04, 0x00, 0x0B}


-- numbers
local wasmFile = {0x00, 0x61, 0x73, 0x6D, 0x01, 0x00, 0x00, 0x00, 0x01, 0x04, 0x01, 0x60, 0x00, 0x00, 0x03, 0x02, 0x01, 0x00, 0x05, 0x03, 0x01, 0x00, 0x01, 0x07, 0x08, 0x01, 0x04, 0x74, 0x65, 0x73, 0x74, 0x00, 0x00, 0x0A, 0x5C, 0x01, 0x5A, 0x00, 0x42, 0x00, 0x1A, 0x42, 0x01, 0x1A, 0x42, 0x02, 0x1A, 0x42, 0xFF, 0x01, 0x1A, 0x42, 0x80, 0x02, 0x1A, 0x42, 0xE8, 0x07, 0x1A, 0x42, 0x95, 0x9A, 0xEF, 0x3A, 0x1A, 0x42, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x1A, 0x42, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x7F, 0x1A, 0x42, 0x81, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x7F, 0x1A, 0x42, 0xB4, 0xE6, 0xCC, 0x99, 0xB3, 0xE6, 0xCC, 0x99, 0x73, 0x1A, 0x42, 0xE9, 0xC6, 0x8F, 0x81, 0x52, 0x1A, 0x42, 0x81, 0x7E, 0x1A, 0x42, 0x7F, 0x1A, 0x0B}

--floats
--local wasmFile = {0x00, 0x61, 0x73, 0x6D, 0x01, 0x00, 0x00, 0x00, 0x01, 0x05, 0x01, 0x60, 0x00, 0x01, 0x7C, 0x03, 0x02, 0x01, 0x00, 0x05, 0x03, 0x01, 0x00, 0x01, 0x07, 0x08, 0x01, 0x04, 0x74, 0x65, 0x73, 0x74, 0x00, 0x00, 0x0A, 0x0D, 0x01, 0x0B, 0x00, 0x44, 0x58, 0x39, 0xB4, 0xC8, 0xD6, 0x1C, 0xC8, 0xC0, 0x0B}

local wasmLen = #wasmFile

-- pre-fill memory array
local mem = {}
for i = 1, 16777216 do
	mem[i] = 0
end

--print(#mem)


--print(18446744073709551616)


--function shl(x, y)
----bit32.lshift()
--return x * y % 2147483648
--end

bit32 = {}
bit32.bor = function(a, b)
	return a | b
end
bit32.bxor = function(a, b)
	return a ^ b
end
bit32.band = function(a, b)
	return a & b
end
bit32.lshift = function(a, b)
	return a << b
end
bit32.rshift = function(a, b)
	return a >> b
end



local pos = 1
local magic = 0x0061736D
local version = 1

--[[
	Bit32 is Unsigned
	0 - 4294967295
]]

-- 1112223334445556667 (signed i64)

-- 10111011  11000111  10011100  11000100  10101101  10100111  11011010  10110111  00001111
-- reverse bytes and remove last bit
-- 0b0001111_0110111_1011010_0100111_0101101_1000100_0011100_1000111_0111011n

function read_LEB(maxbits, sign)
	local result = 0
	local shift = 0
	local bcnt = 0
	local byte = 0
	while true do
		byte = wasmFile[pos]
		pos = pos + 1
		result = bit32.bor(result, bit32.lshift(bit32.band(byte, 0x7F), shift))
		shift = shift + 7
		if bit32.band(byte, 0x80) == 0 then
			break
		end
		bcnt = bcnt + 1
		if bcnt > (maxbits + 7 - 1) / 7 then
			print("Error: Invalid LEB sequence")
		end
	end
	if sign and shift < maxbits and bit32.band(byte, 0x40) then
		result = bit32.bor(result, -bit32.lshift(1, shift))
		result = result - 4294967296
	end
	return result
end

function read_LEB64(signed)
	local res = {0, 0, 0, 0, 0, 0, 0, 0} -- BE
	local overflow = 0
	local bitpos = 0
	local byte = 0
	while true do
		byte = wasmFile[pos]
		pos = pos + 1
		local val = bit32.band(byte, 0x7F)
		local shiftpos = bitpos % 8
		local rempos = 8 - shiftpos
		local bytepos = math.floor(bitpos / 8)
		local aridx = 7 - bytepos + 1
		if bitpos < 64 then -- integrate first part
			res[aridx] = bit32.bor(res[aridx], bit32.band(bit32.lshift(val, shiftpos), 0xFF))
		end
		if rempos < 8 then
			local idx = 7 - (bytepos + 1) + 1
			if idx >= 1 then -- integrate remaining part
				res[idx] = bit32.bor(res[idx], bit32.rshift(val, rempos))
			end
		end
		bitpos = bitpos + 7
		if bit32.band(byte, 0x80) == 0 then
			if bitpos < 64 and bit32.band(byte, 0x40) ~= 0 then -- negative number
				local bitcount = 64 - bitpos
				local iter = 1
				while bitcount > 0 do -- extend negated bits
					local max = bitcount
					if max > 8 then
						max = 8
					end
					local xor = bit32.band(bit32.lshift(255, 8 - max), 0xFF)
					res[iter] = bit32.bxor(res[iter], xor)
					iter = iter + 1
					bitcount = bitcount - 8
				end
			end
			break
		end
	end
	return res
end


function readU32(le)
	local num = 0
	if le then
		num = wasmFile[pos] * 16777216 + wasmFile[pos + 1] * 65536 + wasmFile[pos + 2] * 256 + wasmFile[pos + 3]
	else
		num = wasmFile[pos + 3] * 16777216 + wasmFile[pos + 2] * 65536 + wasmFile[pos + 1] * 256 + wasmFile[pos]
	end
	pos = pos + 4
	return num
end

function readString()
	local str_len = read_LEB(32, false)
	local poff = pos
	pos = pos + str_len
	local str = ""
	for i = 0, str_len - 1 do
		str = str .. string.char(wasmFile[poff + i])
	end
	return str
end

function add(x, y)
	local res = {0, 0, 0, 0, 0, 0, 0, 0, 0}
	local carry = 0
	for i = 0, 7 do
		local pos = 7 - i + 1
		local sum = x[pos] + y[pos] + carry
		res[pos + 1] = sum % 256
		carry = math.floor(sum / 256)
	end
	res[1] = carry
	return res
end

function sub(x, y)
	local res = {0, 0, 0, 0, 0, 0, 0, 0, 0}
	local borrow = 0
	for i = 0, 7 do
		local pos = 7 - i + 1
		local diff = x[pos] - y[pos] + borrow;
		res[pos + 1] = diff - math.floor(diff / 256) * 256
		borrow = math.floor(diff / 256)
	end
	res[1] = borrow - math.floor(borrow / 256) * 256
	return res
end

function mul(x, y)
	local res = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	local add_buffer = {0, 0, 0, 0, 0, 0, 0, 0, 0}
	local add_carry = 0
	local carry = 0
	for b = 0, 7 do
		local carry = 0
		for a = 0, 7 do
			local posA = 7 - a
			local posB = 7 - b
			local prod = x[posA + 1] * y[posB + 1] + carry
			carry = math.floor(prod / 256)
			add_buffer[8 - a + 1] = prod % 256
		end
		add_buffer[1] = carry
		add_carry = 0
		for i = 0, 8 do
			local pos = 15 - i - b + 1
			local sum = res[pos] + add_buffer[8 - i + 1] + add_carry
			res[pos] = sum % 256
			add_carry = math.floor(sum / 256)
		end
	end
	return res
end

function convertBin(binObj, bytObj)
	for i = 0, #bytObj - 1 do
		local pos = i * 8
		local byt = bytObj[i + 1]
		binObj[pos + 1] = bit32.band(bit32.rshift(byt, 7), 1)
		binObj[pos + 2] = bit32.band(bit32.rshift(byt, 6), 1)
		binObj[pos + 3] = bit32.band(bit32.rshift(byt, 5), 1)
		binObj[pos + 4] = bit32.band(bit32.rshift(byt, 4), 1)
		binObj[pos + 5] = bit32.band(bit32.rshift(byt, 3), 1)
		binObj[pos + 6] = bit32.band(bit32.rshift(byt, 2), 1)
		binObj[pos + 7] = bit32.band(bit32.rshift(byt, 1), 1)
		binObj[pos + 8] = bit32.band(bit32.rshift(byt, 0), 1)
	end
end

local divX = {
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
local divY = {
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
local res = {
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
local resoff = 0 -- length of result
local carryDown = {
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
local cdLen = 0
local subBuffer = {
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
function div() -- dividend: divX, divisor: divY, result: res (len: resoff), remainder: carryDown (len: cdLen)
	-- dividend / divisor = quotient
	resoff = 0
	cdLen = 0
	local cdOne = false
	local resOne = false
	local xoff = 0
	local digits = 0
	local xDigits = 0
	for i = 1, #divY do
		if divY[i] ~= 0 then break end
		digits = digits + 1
	end
	for i = 1, #divX do
		if divX[i] ~= 0 then break end
		xDigits = xDigits + 1
	end
	digits = 64 - digits
	if digits == 0 then
		return -1 -- division by 0
	end
	xDigits = 64 - xDigits
	if xDigits == 0 then
		res[1] = 0
		resoff = 1
		return 0
	end
	for i = 1, 64 do
		if xoff >= xDigits then -- no more digits
			break
		end
		xoff = xoff + 1
		local nd = divX[(64 - xDigits) + xoff]
		if nd ~= 0 then -- don't reserve space for zeros at beginning
			cdOne = true
		end
		if cdOne ~= 0 then
			cdLen = cdLen + 1
			carryDown[cdLen] = nd -- bring down
		end
		local cmp = 1
		if cdLen == digits then
			for d = 1, cdLen do
				local a = carryDown[d]
				local b = divY[64 - digits + d]
				if a > b then
					break
				elseif a < b then
					cmp = 0
					break
				end
			end
		elseif cdLen < digits then
			cmp = 0
		end
		-- check divisible (carryDown / divisor)
		if cmp == 0 then -- carryDown < divisor
			if resOne then
				resoff = resoff + 1
				res[resoff] = 0
			end
		else -- carryDown >= divisor
			resoff = resoff + 1
			resOne = true
			res[resoff] = 1
			local carry = 0
			local subLen = 0
			for s = 0, cdLen - 1 do
				local pos = cdLen - s
				local dx = 1 - carryDown[pos]
				local dy = divY[63 - s + 1]
				local sum = bit32.bxor(dx, dy)
				local diff = 1 - bit32.bxor(sum, carry)
				subBuffer[pos] = diff
				if diff ~= 0 then
					subLen = s + 1
				end
				carry = bit32.bor(bit32.band(sum, carry), bit32.band(dx, dy))
			end
			-- replace carryDown with difference
			for s = 1, cdLen do
				carryDown[s] = 0
			end
			cdOne = false
			for s = 1, subLen do
				carryDown[s] = subBuffer[s + (cdLen - subLen)]
				if carryDown[s] ~= 0 then
					cdOne = true
				end
			end
			cdLen = subLen
		end
	end
	return 0
end

--[[
div()
local str = ""
for i = 1, resoff do
	str = str .. tostring(res[i])
end
print(str)]]

local infval = 1 / 0
local nanval = 0 / 0

function convertFloat(b) -- LE
	local sign = bit32.rshift(bit32.band(b[4], 0x80), 7)
	local mantissa = bit32.bor(bit32.band(bit32.lshift(b[4], 1), 0xFF), bit32.rshift(b[3], 7)) - 127
	local exp = 2 ^ mantissa
	local frac = (b[1] + b[2] * 256 + bit32.band(b[3], 0x7F) * 65536) / bit32.lshift(2, 22)
	local number = exp * (1 + frac)
	if mantissa == -127 and frac ~= 0 then -- denormalized
		number = 2 ^ -126 * frac
	end
	if mantissa == 128 and frac == 0 then
		number = infval
	end
	if mantissa == 128 and frac ~= 0 then
		number = nanval
	end
	if sign == 1 then
		number = -number
	end
	return number
end

function convertDouble(b) -- LE
	local sign = bit32.rshift(bit32.band(b[8], 0x80), 7)
	local mantissa = bit32.bor(bit32.lshift(bit32.band(b[8], 0x7F), 4), bit32.rshift(b[7], 4)) - 1023
	local exp = 2 ^ mantissa
	local frac = (b[1] +
		b[2] * 256 +
		b[3] * 65536 +
		b[4] * 16777216 +
		b[5] * 4294967296 +
		b[6] * 1099511627776 +
		bit32.band(b[7], 0x0F) * 281474976710656) / 4503599627370496
	local number = exp * (1 + frac)
	if mantissa == -1023 and frac ~= 0 then -- denormalized
		number = 2 ^ -1022 * frac
	end
	if mantissa == 1024 and frac == 0 then
		number = infval
	end
	if mantissa == 1024 and frac ~= 0 then
		number = nanval
	end
	if sign == 1 then
		number = -number
	end
	return number
end





local fMagic = readU32(true)
local fVersion = readU32()

if fMagic ~= magic or fVersion ~= version then
	print("Error: Invalid header or version")
end





local KIND_FUNCTION = 0
local KIND_TABLE    = 1
local KIND_MEMORY   = 2
local KIND_GLOBAL   = 3

local kLocalVoid = 0x40 -- 64
local kLocalI32 = 0x7F -- 127
local kLocalI64 = 0x7E -- 126
local kLocalF32 = 0x7D -- 125
local kLocalF64 = 0x7C -- 124
local kLocalS128 = 0x7B -- 123
local kLocalAnyFunc = 0x70 -- 112
local kLocalAnyRef = 0x6F -- 111
local kLocalExceptRef = 0x68 -- 104

local kWasmStmt = 0
local kWasmI32 = 1
local kWasmI64 = 2
local kWasmF32 = 3
local kWasmF64 = 4
local kWasmS128 = 5
local kWasmAnyRef = 6
local kWasmAnyFunc = 7
local kWasmNullRef = 8
local kWasmExceptRef = 9
local kWasmVar = 10

function convert_value_type(t) 
	if t == kLocalI32 then
		return kWasmI32
	elseif t == kLocalI64 then
		return kWasmI64
	elseif t == kLocalF32 then
		return kWasmF32
	elseif t == kLocalF64 then
		return kWasmF64
	end
	return kWasmStmt
end

-- imported from stack overflow
function tableToString (tbl, indent)
	if not indent then indent = 0 end
	local toprint = string.rep(" ", indent) .. "{\n"
	indent = indent + 2 
	for k, v in pairs(tbl) do
		toprint = toprint .. string.rep(" ", indent)
		if (type(k) == "number") then
			toprint = toprint .. "[" .. k .. "] = "
		elseif (type(k) == "string") then
			toprint = toprint  .. k ..  "= "   
		end
		if (type(v) == "number") then
			toprint = toprint .. v .. ",\n"
		elseif (type(v) == "string") then
			toprint = toprint .. "\"" .. v .. "\",\n"
		elseif (type(v) == "table") then
			toprint = toprint .. tableToString(v, indent + 2) .. ",\n"
		else
			toprint = toprint .. "\"" .. tostring(v) .. "\",\n"
		end
	end
	toprint = toprint .. string.rep(" ", indent-2) .. "}"
	return toprint
end

function byteSeqToHex(bytes)
	local res = "0x"
	local len = #bytes
	for i = 1, len do
		local byt = string.format("%02X", bytes[i])
		if #byt == 1 then
			byt = "0" .. byt
		end
		res = res .. byt
	end
	return res
end

local stack = {}
local stackPos = 1

function debugPrint(str)
	print(str)
end

function processInstruction()
	local ins = wasmFile[pos]
	pos = pos + 1

	if ins == 0x00 then
		debugPrint("nop")
	elseif ins == 0x01 then
		local btype = read_LEB(32, false)
		debugPrint("block type=" .. btype)
	elseif ins == 0x03 then
		local btype = read_LEB(32, false)
		debugPrint("loop type=" .. btype)
	elseif ins == 0x04 then
		local btype = read_LEB(32, false)
		debugPrint("if type=" .. btype)
	elseif ins == 0x05 then
		debugPrint("else")
	elseif ins == 0x0B then
		debugPrint("end")
	elseif ins == 0x0C then
		local depth = read_LEB(32, false)
		debugPrint("br depth=" .. depth)
	elseif ins == 0x0D then
		local depth = read_LEB(32, false)
		debugPrint("br_if depth=" .. depth)
	elseif ins == 0x0E then
		local count = read_LEB(32, false)
		for z = 1, count do
			local tbl = read_LEB(32, false)
			debugPrint("br table " .. tbl)
		end
		local depth = read_LEB(32, false)
		debugPrint("br_table depth=" .. depth)
	elseif ins == 0x0F then
		debugPrint("return")
	elseif ins == 0x10 then
		local fidx = read_LEB(32, false)
		debugPrint("call idx=" .. fidx)
	elseif ins == 0x11 then
		local tidx = read_LEB(32, false)
		local imm = read_LEB(32, false)
		debugPrint("call_indirect idx=" .. tidx .. " imm=" .. imm)
	elseif ins == 0x1A then
		debugPrint("drop")
	elseif ins == 0x1B then
		debugPrint("select")
	elseif ins == 0x20 then
		local arg = read_LEB(32, false)
		debugPrint("local.get arg=" .. arg)
	elseif ins == 0x21 then
		local arg = read_LEB(32, false)
		debugPrint("local.set arg=" .. arg)
	elseif ins == 0x22 then
		local arg = read_LEB(32, false)
		debugPrint("local.tee arg=" .. arg)
	elseif ins == 0x23 then
		local arg = read_LEB(32, false)
		debugPrint("global.get arg=" .. arg)
	elseif ins == 0x24 then
		local arg = read_LEB(32, false)
		debugPrint("global.set arg=" .. arg)
	elseif ins == 0x25 then
		local arg = read_LEB(32, false)
		debugPrint("table.get arg=" .. arg)
	elseif ins == 0x26 then
		local arg = read_LEB(32, false)
		debugPrint("table.set arg=" .. arg)
	elseif ins == 0x3F then
		local res = read_LEB(32, false)
		debugPrint("current_memory res=" .. res)
	elseif ins == 0x40 then
		local res = read_LEB(32, false)
		debugPrint("grow_memory res=" .. res)
	elseif ins >= 0x28 and ins <= 0x35 then
		local flags = read_LEB(32, false)
		local offset = read_LEB(32, false)
		debugPrint("MEM LOAD")
	elseif ins >= 0x36 and ins <= 0x3E then
		local flags = read_LEB(32, false)
		local offset = read_LEB(32, false)
		if ins == 0x36 then
			debugPrint("i32.store")
		elseif ins == 0x37 then
			debugPrint("i64.store")
		elseif ins == 0x38 then
			debugPrint("f32.store")
		elseif ins == 0x39 then
			debugPrint("f64.store")
		elseif ins == 0x3A then
			debugPrint("i32.store8")
		elseif ins == 0x3B then
			debugPrint("i32.store16")
		elseif ins == 0x3C then
			debugPrint("i64.store8")
		elseif ins == 0x3D then
			debugPrint("i64.store16")
		elseif ins == 0x3E then
			debugPrint("i64.store32")
		end
	elseif ins == 0x41 then
		local arg = read_LEB(32, true)
		stack[stackPos] = {
			vtype = "i32",
			val = arg
		}
		stackPos = stackPos + 1
		debugPrint("i32.const arg=" .. arg)
	elseif ins == 0x42 then
		local arg = read_LEB64(true)
		stack[stackPos] = {
			vtype = "i64",
			val = arg
		}
		stackPos = stackPos + 1
		debugPrint("i64.const arg=" .. byteSeqToHex(arg))
	elseif ins == 0x43 then
		local num = {wasmFile[pos], wasmFile[pos + 1], wasmFile[pos + 2], wasmFile[pos + 3]}
		local fnum = convertFloat(num)
		pos = pos + 4
		debugPrint("f32.const arg=" .. fnum)
	elseif ins == 0x44 then
		local num = {wasmFile[pos], wasmFile[pos + 1], wasmFile[pos + 2], wasmFile[pos + 3],
			wasmFile[pos + 4], wasmFile[pos + 5], wasmFile[pos + 6], wasmFile[pos + 7]}
		local dnum = convertDouble(num)
		pos = pos + 8
		debugPrint("f64.const arg=" .. dnum)
	elseif ins == 0x45 then
		debugPrint("i32.eqz")
	elseif ins == 0x50 then
		debugPrint("i64.eqz")
	elseif ins >= 0x46 and ins <= 0x4F then
		debugPrint("i32 binary")
	elseif ins >= 0x51 and ins <= 0x5A then
		debugPrint("i64 binary")
	elseif ins >= 0x5B and ins <= 0x60 then
		debugPrint("f32 binary")
	elseif ins >= 0x61 and ins <= 0x66 then
		debugPrint("f64 binary")
	elseif ins >= 0x67 and ins <= 0x69 then
		debugPrint("i32 unary")
	elseif ins >= 0x79 and ins <= 0x7B then
		debugPrint("i64 unary")
	elseif ins >= 0x8B and ins <= 0x91 then
		debugPrint("f32 unary")
	elseif ins >= 0x99 and ins <= 0x9F then
		debugPrint("f64 unary")
	elseif ins >= 0x6A and ins <= 0x78 then
		debugPrint("i32 binary 2")
	elseif ins >= 0x7C and ins <= 0x8A then
		debugPrint("i64 binary 2")
	elseif ins >= 0x92 and ins <= 0x98 then
		debugPrint("f32 binary 2")
	elseif ins >= 0xA0 and ins <= 0xA6 then
		debugPrint("f64 binary 2")
	elseif ins >= 0xA7 and ins <= 0xBF then
		debugPrint("MISCELLANEOUS")
	elseif ins == 0xC0 then
		debugPrint("i32.extend8_s")
	elseif ins == 0xC1 then
		debugPrint("i32.extend16_s")
	elseif ins == 0xC2 then
		debugPrint("i64.extend8_s")
	elseif ins == 0xC3 then
		debugPrint("i64.extend16_s")
	elseif ins == 0xC4 then
		debugPrint("i64.extend32_s")
	elseif ins == 0xD0 then
		local reftype = read_LEB(32, false)
		debugPrint("ref.null reftype=" .. reftype)
	elseif ins == 0xD1 then
		debugPrint("ref.is_null")
	elseif ins == 0xD2 then
		local funcidx = read_LEB(32, false)
		debugPrint("ref.func funcidx=" .. funcidx)
	elseif ins == 0xD5 then -- taken from firefox, comparison op
		local xyz = read_LEB(32, false)
		debugPrint("ref.eq xyz=" .. xyz)
	elseif ins == 0xFB then
		ins = wasmFile[pos]
		pos = pos + 1
		debugPrint("Garbage Collection Instruction op=" .. ins)
	elseif ins == 0xFC then
		ins = wasmFile[pos]
		pos = pos + 1
		if ins == -1 then
			-- 0x00 - 0x07 in flexible-vectors specs
		elseif ins == 0x00 then
			debugPrint("i32.trunc_sat_f32_s")
		elseif ins == 0x01 then
			debugPrint("i32.trunc_sat_f32_u")
		elseif ins == 0x02 then
			debugPrint("i32.trunc_sat_f64_s")
		elseif ins == 0x03 then
			debugPrint("i32.trunc_sat_f64_u")
		elseif ins == 0x04 then
			debugPrint("i64.trunc_sat_f32_s")
		elseif ins == 0x05 then
			debugPrint("i64.trunc_sat_f32_u")
		elseif ins == 0x06 then
			debugPrint("i64.trunc_sat_f64_s")
		elseif ins == 0x07 then
			debugPrint("i64.trunc_sat_f64_u")
		elseif ins == 0x08 then
			local idx = read_LEB(32, false)
			read_LEB(32, false)
			debugPrint("memory.init idx=" .. idx)
		elseif ins == 0x09 then
			local idx = read_LEB(32, false)
			debugPrint("data.drop idx=" .. idx)
		elseif ins == 0x0A then
			read_LEB(32, false)
			read_LEB(32, false)
			debugPrint("memory.copy")
		elseif ins == 0x0B then
			read_LEB(32, false)
			debugPrint("memory.fill")
		elseif ins == 0x0C then
			local elemidx = read_LEB(32, false)
			local tableidx = read_LEB(32, false)
			-- accepts 2 arguments, but 3 more arguments are on the stack
			debugPrint("table.init elemidx=" .. elemidx .. " tableidx=" .. tableidx)
		elseif ins == 0x0D then
			local arg = read_LEB(32, false)
			debugPrint("elem.drop arg=" .. arg)
		elseif ins == 0x0E then
			local x = read_LEB(32, false)
			local y = read_LEB(32, false)
			debugPrint("table.copy x=" .. x .. " y=" .. y)
		elseif ins == 0x0F then
			local arg = read_LEB(32, false)
			debugPrint("table.grow arg=" .. arg)
		elseif ins == 0x10 then
			local arg = read_LEB(32, false)
			debugPrint("table.size arg=" .. arg)
		elseif ins == 0x11 then
			local arg = read_LEB(32, false)
			debugPrint("table.fill arg=" .. arg)
		end
	elseif ins == 0xFD then
		ins = read_LEB(32, false) -- opcode is variable length
		-- https://github.com/WebAssembly/simd/blob/master/proposals/simd/NewOpcodes.md
		-- https://webassembly.github.io/simd/core/_download/WebAssembly.pdf
		-- https://github.com/tc39/ecmascript_simd/blob/master/src/ecmascript_simd.js
		if ins == 0x00 then
			local align = read_LEB(32, false) -- typically 4 (2^4=16)
			local offset = read_LEB(32, false) -- typically 0 (added to mem address)
			-- memory address is last i32 in stack
			debugPrint("v128.load align=" .. align .. " offset=" .. offset)
		elseif ins == 0x01 then
			local align = read_LEB(32, false)
			local offset = read_LEB(32, false)
			debugPrint("v128.load8x8_s align=" .. align .. " offset=" .. offset)
		elseif ins == 0x02 then
			local align = read_LEB(32, false)
			local offset = read_LEB(32, false)
			debugPrint("v128.load8x8_u align=" .. align .. " offset=" .. offset)
		elseif ins == 0x03 then
			local align = read_LEB(32, false)
			local offset = read_LEB(32, false)
			debugPrint("v128.load16x4_s align=" .. align .. " offset=" .. offset)
		elseif ins == 0x04 then
			local align = read_LEB(32, false)
			local offset = read_LEB(32, false)
			debugPrint("v128.load16x4_u align=" .. align .. " offset=" .. offset)
		elseif ins == 0x05 then
			local align = read_LEB(32, false)
			local offset = read_LEB(32, false)
			debugPrint("v128.load32x2_s align=" .. align .. " offset=" .. offset)
		elseif ins == 0x06 then
			local align = read_LEB(32, false)
			local offset = read_LEB(32, false)
			debugPrint("v128.load32x2_u align=" .. align .. " offset=" .. offset)
		elseif ins == 0x07 then
			local align = read_LEB(32, false)
			local offset = read_LEB(32, false)
			debugPrint("v128.load8_splat align=" .. align .. " offset=" .. offset)
		elseif ins == 0x08 then
			local align = read_LEB(32, false)
			local offset = read_LEB(32, false)
			debugPrint("v128.load16_splat align=" .. align .. " offset=" .. offset)
		elseif ins == 0x09 then
			local align = read_LEB(32, false)
			local offset = read_LEB(32, false)
			debugPrint("v128.load32_splat align=" .. align .. " offset=" .. offset)
		elseif ins == 0x0A then
			local align = read_LEB(32, false)
			local offset = read_LEB(32, false)
			debugPrint("v128.load64_splat align=" .. align .. " offset=" .. offset)
		elseif ins == 0x0B then
			local align = read_LEB(32, false)
			local offset = read_LEB(32, false)
			debugPrint("v128.store align=" .. align .. " offset=" .. offset)
		elseif ins == 0x0C then
			local bytes = { -- i128 val
				wasmFile[pos], wasmFile[pos+1], wasmFile[pos+2], wasmFile[pos+3],
				wasmFile[pos+4], wasmFile[pos+5], wasmFile[pos+5], wasmFile[pos+7],
				wasmFile[pos+8], wasmFile[pos+9], wasmFile[pos+10], wasmFile[pos+11],
				wasmFile[pos+12], wasmFile[pos+13], wasmFile[pos+14], wasmFile[pos+15]
			}
			debugPrint("v128.const val(i128)=" .. byteSeqToHex(bytes))
			pos = pos + 16
		elseif ins == 0x0D then
			local bytes = {
				wasmFile[pos], wasmFile[pos+1], wasmFile[pos+2], wasmFile[pos+3],
				wasmFile[pos+4], wasmFile[pos+5], wasmFile[pos+5], wasmFile[pos+7],
				wasmFile[pos+8], wasmFile[pos+9], wasmFile[pos+10], wasmFile[pos+11],
				wasmFile[pos+12], wasmFile[pos+13], wasmFile[pos+14], wasmFile[pos+15]
			}
			debugPrint("i8x16.shuffle val=" .. byteSeqToHex(bytes))
			pos = pos + 16
		elseif ins == 0x0E then
			debugPrint("i8x16.swizzle")
		elseif ins == 0x0F then
			debugPrint("i8x16.splat")
		elseif ins == 0x10 then
			debugPrint("i16x8.splat")
		elseif ins == 0x11 then
			debugPrint("i32x4.splat")
		elseif ins == 0x12 then
			debugPrint("i64x2.splat")
		elseif ins == 0x13 then
			debugPrint("f32x4.splat")
		elseif ins == 0x14 then
			debugPrint("f64x2.splat")
		elseif ins == 0x15 then
			local byt = wasmFile[pos]
			pos = pos + 1
			debugPrint("i8x16.extract_lane_s byte=" + byt)
		elseif ins == 0x16 then
			local byt = wasmFile[pos]
			pos = pos + 1
			debugPrint("i8x16.extract_lane_u byte=" + byt)
		elseif ins == 0x17 then
			local byt = wasmFile[pos]
			pos = pos + 1
			debugPrint("i8x16.replace_lane byte=" + byt)
		elseif ins == 0x18 then
			local byt = wasmFile[pos]
			pos = pos + 1
			debugPrint("i16x8.extract_lane_s byte=" + byt)
		elseif ins == 0x19 then
			local byt = wasmFile[pos]
			pos = pos + 1
			debugPrint("i16x8.extract_lane_u byte=" + byt)
		elseif ins == 0x1A then
			local byt = wasmFile[pos]
			pos = pos + 1
			debugPrint("i16x8.replace_lane byte=" + byt)
		elseif ins == 0x1B then
			local byt = wasmFile[pos]
			pos = pos + 1
			debugPrint("i32x4.extract_lane byte=" + byt)
		elseif ins == 0x1C then
			local byt = wasmFile[pos]
			pos = pos + 1
			debugPrint("i32x4.replace_lane byte=" + byt)
		elseif ins == 0x1D then
			local byt = wasmFile[pos]
			pos = pos + 1
			debugPrint("i64x2.extract_lane byte=" + byt)
		elseif ins == 0x1E then
			local byt = wasmFile[pos]
			pos = pos + 1
			debugPrint("i64x2.replace_lane byte=" + byt)
		elseif ins == 0x1F then
			local byt = wasmFile[pos]
			pos = pos + 1
			debugPrint("f32x4.extract_lane byte=" + byt)
		elseif ins == 0x20 then
			local byt = wasmFile[pos]
			pos = pos + 1
			debugPrint("f32x4.replace_lane byte=" + byt)
		elseif ins == 0x21 then
			local byt = wasmFile[pos]
			pos = pos + 1
			debugPrint("f64x2.extract_lane byte=" + byt)
		elseif ins == 0x22 then
			local byt = wasmFile[pos]
			pos = pos + 1
			debugPrint("f64x2.replace_lane byte=" + byt)
		elseif ins == 0x23 then
			debugPrint("i8x16.eq")
		elseif ins == 0x24 then
			debugPrint("i8x16.ne")
		elseif ins == 0x25 then
			debugPrint("i8x16.lt_s")
		elseif ins == 0x26 then
			debugPrint("i8x16.lt_u")
		elseif ins == 0x27 then
			debugPrint("i8x16.gt_s")
		elseif ins == 0x28 then
			debugPrint("i8x16.gt_u")
		elseif ins == 0x29 then
			debugPrint("i8x16.le_s")
		elseif ins == 0x2A then
			debugPrint("i8x16.le_u")
		elseif ins == 0x2B then
			debugPrint("i8x16.ge_s")
		elseif ins == 0x2C then
			debugPrint("i8x16.ge_u")
		elseif ins == 0x2D then
			debugPrint("i16x8.eq")
		elseif ins == 0x2E then
			debugPrint("i16x8.ne")
		elseif ins == 0x2F then
			debugPrint("i16x8.lt_s")
		elseif ins == 0x30 then
			debugPrint("i16x8.lt_u")
		elseif ins == 0x31 then
			debugPrint("i16x8.gt_s")
		elseif ins == 0x32 then
			debugPrint("i16x8.gt_u")
		elseif ins == 0x33 then
			debugPrint("i16x8.le_s")
		elseif ins == 0x34 then
			debugPrint("i16x8.le_u")
		elseif ins == 0x35 then
			debugPrint("i16x8.ge_s")
		elseif ins == 0x36 then
			debugPrint("i16x8.ge_u")
		elseif ins == 0x37 then
			debugPrint("i32x4.eq")
		elseif ins == 0x38 then
			debugPrint("i32x4.ne")
		elseif ins == 0x39 then
			debugPrint("i32x4.lt_s")
		elseif ins == 0x3A then
			debugPrint("i32x4.lt_u")
		elseif ins == 0x3B then
			debugPrint("i32x4.gt_s")
		elseif ins == 0x3C then
			debugPrint("i32x4.gt_u")
		elseif ins == 0x3D then
			debugPrint("i32x4.le_s")
		elseif ins == 0x3E then
			debugPrint("i32x4.le_u")
		elseif ins == 0x3F then
			debugPrint("i32x4.ge_s")
		elseif ins == 0x40 then
			debugPrint("i32x4.ge_u")
		elseif ins == 0x41 then
			debugPrint("f32x4.eq")
		elseif ins == 0x42 then
			debugPrint("f32x4.ne")
		elseif ins == 0x43 then
			debugPrint("f32x4.lt")
		elseif ins == 0x44 then
			debugPrint("f32x4.gt")
		elseif ins == 0x45 then
			debugPrint("f32x4.le")
		elseif ins == 0x46 then
			debugPrint("f32x4.ge")
		elseif ins == 0x47 then
			debugPrint("f64x2.eq")
		elseif ins == 0x48 then
			debugPrint("f64x2.ne")
		elseif ins == 0x49 then
			debugPrint("f64x2.lt")
		elseif ins == 0x4A then
			debugPrint("f64x2.gt")
		elseif ins == 0x4B then
			debugPrint("f64x2.le")
		elseif ins == 0x4C then
			debugPrint("f64x2.ge")
		elseif ins == 0x4D then
			debugPrint("v128.not")
		elseif ins == 0x4E then
			debugPrint("v128.and")
		elseif ins == 0x4F then
			debugPrint("v128.andnot")
		elseif ins == 0x50 then
			debugPrint("v128.or")
		elseif ins == 0x51 then
			debugPrint("v128.xor")
		elseif ins == 0x52 then
			debugPrint("v128.bitselect")
			-- skip 0x53-0x5F
		elseif ins == 0x60 then
			debugPrint("i8x16.abs")
		elseif ins == 0x61 then
			debugPrint("i8x16.neg")
		elseif ins == 0x62 then
			debugPrint("i8x16.any_true")
		elseif ins == 0x63 then
			debugPrint("i8x16.all_true")
		elseif ins == 0x64 then
			debugPrint("i8x16.bitmask")
		elseif ins == 0x65 then
			debugPrint("i8x16.narrow_i16x8_s")
		elseif ins == 0x66 then
			debugPrint("i8x16.narrow_i16x8_u")
			-- skip 0x67-0x6A (widen)
		elseif ins == 0x6B then
			debugPrint("i8x16.shl")
		elseif ins == 0x6C then
			debugPrint("i8x16.shr_s")
		elseif ins == 0x6D then
			debugPrint("i8x16.shr_u")
		elseif ins == 0x6E then
			debugPrint("i8x16.add")
		elseif ins == 0x6F then
			debugPrint("i8x16.add_saturate_s")
		elseif ins == 0x70 then
			debugPrint("i8x16.add_saturate_u")
		elseif ins == 0x71 then
			debugPrint("i8x16.sub")
		elseif ins == 0x72 then
			debugPrint("i8x16.sub_saturate_s")
		elseif ins == 0x73 then
			debugPrint("i8x16.sub_saturate_u")
			-- skip 0x74 (dot)
			-- skip 0x75 (mul)
		elseif ins == 0x76 then
			debugPrint("i8x16.min_s")
		elseif ins == 0x77 then
			debugPrint("i8x16.min_u")
		elseif ins == 0x78 then
			debugPrint("i8x16.max_s")
		elseif ins == 0x79 then
			debugPrint("i8x16.max_u")
			-- skip 0x7A (avgr_s)
		elseif ins == 0x7B then
			debugPrint("i8x16.avgr_u")
			-- skip 0x7C-0x7F
		elseif ins == 0x80 then
			debugPrint("i16x8.abs")
		elseif ins == 0x81 then
			debugPrint("i16x8.neg")
		elseif ins == 0x82 then
			debugPrint("i16x8.any_true")
		elseif ins == 0x83 then
			debugPrint("i16x8.all_true")
		elseif ins == 0x84 then
			debugPrint("i16x8.bitmask")
		elseif ins == 0x85 then
			debugPrint("i16x8.narrow_i32x4_s")
		elseif ins == 0x86 then
			debugPrint("i16x8.narrow_i32x4_u")
		elseif ins == 0x87 then
			debugPrint("i16x8.widen_low_i8x16_s")
		elseif ins == 0x88 then
			debugPrint("i16x8.widen_high_i8x16_s")
		elseif ins == 0x89 then
			debugPrint("i16x8.widen_low_i8x16_u")
		elseif ins == 0x8A then
			debugPrint("i16x8.widen_high_i8x16_u")
		elseif ins == 0x8B then
			debugPrint("i16x8.shl")
		elseif ins == 0x8C then
			debugPrint("i16x8.shr_s")
		elseif ins == 0x8D then
			debugPrint("i16x8.shr_u")
		elseif ins == 0x8E then
			debugPrint("i16x8.add")
		elseif ins == 0x8F then
			debugPrint("i16x8.add_saturate_s")
		elseif ins == 0x90 then
			debugPrint("i16x8.add_saturate_u")
		elseif ins == 0x91 then
			debugPrint("i16x8.sub")
		elseif ins == 0x92 then
			debugPrint("i16x8.sub_saturate_s")
		elseif ins == 0x93 then
			debugPrint("i16x8.sub_saturate_u")
			-- skip 0x94 (dot)
		elseif ins == 0x95 then
			debugPrint("i16x8.mul")
		elseif ins == 0x96 then
			debugPrint("i16x8.min_s")
		elseif ins == 0x97 then
			debugPrint("i16x8.min_u")
		elseif ins == 0x98 then
			debugPrint("i16x8.max_s")
		elseif ins == 0x99 then
			debugPrint("i16x8.max_u")
			-- skip 0x9A (avgr_s)
		elseif ins == 0x9B then
			debugPrint("i16x8.avgr_u")
			-- skip 0x9C-0x9F
		elseif ins == 0xA0 then
			debugPrint("i32x4.abs")
		elseif ins == 0xA1 then
			debugPrint("i32x4.neg")
		elseif ins == 0xA2 then
			debugPrint("i32x4.any_true")
		elseif ins == 0xA3 then
			debugPrint("i32x4.all_true")
		elseif ins == 0xA4 then
			debugPrint("i32x4.bitmask")
			-- skip 0xA5-0xA6 (narrow)
		elseif ins == 0xA7 then
			debugPrint("i32x4.widen_low_i16x8_s")
		elseif ins == 0xA8 then
			debugPrint("i32x4.widen_high_i16x8_s")
		elseif ins == 0xA9 then
			debugPrint("i32x4.widen_low_i16x8_u")
		elseif ins == 0xAA then
			debugPrint("i32x4.widen_high_i16x8_u")
		elseif ins == 0xAB then
			debugPrint("i32x4.shl")
		elseif ins == 0xAC then
			debugPrint("i32x4.shr_s")
		elseif ins == 0xAD then
			debugPrint("i32x4.shr_u")
		elseif ins == 0xAE then
			debugPrint("i32x4.add")
			-- skip 0xAF-0xB0 (add_sat)
		elseif ins == 0xB1 then
			debugPrint("i32x4.sub")
			-- skip 0xB2-0xB3 (sub_sat)
			-- skip 0xB4 (dot)
		elseif ins == 0xB5 then
			debugPrint("i32x4.mul")
		elseif ins == 0xB6 then
			debugPrint("i32x4.min_s")
		elseif ins == 0xB7 then
			debugPrint("i32x4.min_u")
		elseif ins == 0xB8 then
			debugPrint("i32x4.max_s")
		elseif ins == 0xB9 then
			debugPrint("i32x4.max_u")
			-- skip 0xBA-0xBB (avgr_s)
			-- skip 0xBC-0xC0
		elseif ins == 0xC1 then
			debugPrint("i64x2.neg")
			-- skip 0xC2-0xCA
		elseif ins == 0xCB then
			debugPrint("i64x2.shl")
		elseif ins == 0xCC then
			debugPrint("i64x2.shr_s")
		elseif ins == 0xCD then
			debugPrint("i64x2.shr_u")
		elseif ins == 0xCE then
			debugPrint("i64x2.add")
			-- skip 0xCF-0xD0
		elseif ins == 0xD1 then
			debugPrint("i64x2.sub")
			-- skip 0xD2-0xD4
		elseif ins == 0xD5 then
			debugPrint("i64x2.mul")
			-- skip 0xD6-0xDF
		elseif ins == 0xE0 then
			debugPrint("f32x4.abs")
		elseif ins == 0xE1 then
			debugPrint("f32x4.neg")
		elseif ins == 0xE2 then
			debugPrint("round")
		elseif ins == 0xE3 then
			debugPrint("f32x4.sqrt")
		elseif ins == 0xE4 then
			debugPrint("f32x4.add")
		elseif ins == 0xE5 then
			debugPrint("f32x4.sub")
		elseif ins == 0xE6 then
			debugPrint("f32x4.mul")
		elseif ins == 0xE7 then
			debugPrint("f32x4.div")
		elseif ins == 0xE8 then
			debugPrint("f32x4.min")
		elseif ins == 0xE9 then
			debugPrint("f32x4.max")
			-- skip 0xEA (pmin)
			-- skip 0xEB (pmax)
		elseif ins == 0xEC then
			debugPrint("f64x2.abs")
		elseif ins == 0xED then
			debugPrint("f64x2.neg")
			-- skip 0xEE (round)
		elseif ins == 0xEF then
			debugPrint("f64x2.sqrt")
		elseif ins == 0xF0 then
			debugPrint("f64x2.add")
		elseif ins == 0xF1 then
			debugPrint("f64x2.sub")
		elseif ins == 0xF2 then
			debugPrint("f64x2.mul")
		elseif ins == 0xF3 then
			debugPrint("f64x2.div")
		elseif ins == 0xF4 then
			debugPrint("f64x2.min")
		elseif ins == 0xF5 then
			debugPrint("f64x2.max")
			-- skip 0xF6 (pmin)
			-- skip 0xF7 (pmax)
		elseif ins == 0xF8 then
			debugPrint("i32x4.trunc_sat_f32x4_s")
		elseif ins == 0xF9 then
			debugPrint("i32x4.trunc_sat_f32x4_u")
		elseif ins == 0xFA then
			debugPrint("f32x4.convert_i32x4_s")
		elseif ins == 0xFB then
			debugPrint("f32x4.convert_i32x4_u")
			-- skip 0xFC-0xFF
		else
			debugPrint("Invalid SIMD opcode: " .. ins)
		end
	elseif ins == 0xFE then
		ins = wasmFile[pos]
		pos = pos + 1
		debugPrint("Thread Instruction op=" .. ins)
	else
		debugPrint("UNRECOGNIZED op=" .. ins)
	end
end

function runFunction(func)
	if func == nil then
		print("Function does not exist")
		return -1
	end

	local codeStart = func[3]
	local codeEnd = func[4]
	
	local savepos = pos
	pos = codeStart
	while pos < codeEnd do
		processInstruction()
	end
end


local types = {}
local functions = {}
local exports = {}
local globals = {}

local m_export_count = 0
local m_import_count = 0
local m_global_count = 0
local m_function_count = 0
local m_start_function = 0

while pos < wasmLen do
	local id = read_LEB(7, false)
	local slen = read_LEB(32, false)
	local start_pos = pos
	
	if id == 0 then -- custom
		print("section=custom")
		local end_pos = pos + slen
		pos = end_pos
	elseif id == 1 then -- type
		print("section=type")
		local end_pos = pos + slen
		
		local signatures_count = read_LEB(32, false)
		for c = 1, signatures_count do
			local wtype = {{}, {}} -- params, returns
			local type_form = read_LEB(7, false)
			local param_count = read_LEB(32, false)
			for i = 1, param_count do
				local param = read_LEB(7, false)
				wtype[1][i] = convert_value_type(param)
			end
			local return_count = read_LEB(32, false)
			for i = 1, return_count do
				-- if >1 return val, use an array
				local ret = read_LEB(7, false)
				wtype[2][i] = convert_value_type(ret)
			end
			types[c] = wtype
		end
		
		pos = end_pos
	elseif id == 2 then -- import
		print("section=import")
		local end_pos = pos + slen
		
		local import_count = read_LEB(32, false)
		for g = 1, import_count do
			local import_module = readString()
			local import_field = readString()
			--print(import_module .. ", " .. import_field)
			local external_kind = wasmFile[pos]
			pos = pos + 1
			local type_index = 0
			if external_kind == KIND_FUNCTION then
				type_index = read_LEB(32, false)
				m_import_count = m_import_count + 1
				m_function_count = m_function_count + 1
				
				local func = {m_function_count, types[type_index + 1], 0, 0, "", true, import_module, import_field}
				functions[m_function_count] = func
				
				print("KIND_FUNCTION " .. tostring(type_index))
			elseif external_kind == KIND_TABLE then
				local elem_type = read_LEB(7, false)
				local flags = read_LEB(32, false)
				local tsize = read_LEB(32, false)
				local maximum = 0
				if bit32.band(flags, 0x01) then
					tsize = read_LEB(32, false)
					if 0x10000 < tsize then
						maximum = 0x10000
					else
						maximum = tsize
					end
				else
					maximum = 0x10000
				end
				print("KIND_TABLE " .. tostring(elem_type) .. "," .. tostring(flags) .. "," .. tostring(tsize))
			elseif external_kind == KIND_MEMORY then
				local flags = read_LEB(32, false)
				local pages = read_LEB(32, false)
				local maximum = 0
				if bit32.band(flags, 0x01) then
					pages = read_LEB(32, false)
					maximum = math.min(0x8000, pages)
				else
					maximum = 0x8000
				end
				print("KIND_MEMORY " .. tostring(flags) .. "," .. tostring(pages) .. "," .. tostring(maximum))
			elseif external_kind == KIND_GLOBAL then
				local content_type = read_LEB(7, false)
				local mutability = read_LEB(7, false)
				m_global_count = m_global_count + 1
				print("KIND_GLOBAL " .. tostring(content_type) .. "," .. tostring(mutability))
			end
		end
		
		pos = end_pos
	elseif id == 3 then -- function
		print("section=function")
		local end_pos = pos + slen
		
		m_function_count = m_function_count + read_LEB(32, false)
		
		
		for f = 1, m_function_count - m_import_count do
			local tidx = read_LEB(32, false)
			local func = {f + m_import_count, types[tidx + 1], 0, 0, "", false, "", ""}
			-- idx, type, start, end, exp name, is import, imp mod, imp field
			functions[f + m_import_count] = func
			print("Function " .. f + m_import_count)
		end
	
	
		--[[for f = m_import_count + 1, m_function_count - 1 do
			local tidx = read_LEB(32, false)
			local func = {f, types[tidx + 1]} -- idx, type
			functions[f] = func
			print("Function " .. tostring(f))
		end]]
		
		--print("ENDPOS " .. pos .. ":" .. end_pos)
		
		pos = end_pos
	elseif id == 4 then -- table
		print("section=table")
		local end_pos = pos + slen
		
		local table_count = read_LEB(32, false)
		print("table_count=" .. table_count)
		for i = 1, table_count do
			local elm_type = read_LEB(7, false)
			local flags = read_LEB(32, false)
			local tsize = read_LEB(32, false)
			local maximum = 0
			print("table. initial=" .. tsize .. " flags=" .. flags)
			if bit32.band(flags, 0x01) then
				tsize = read_LEB(32, false)
				if tsize > 0x10000 then
					maximum = 0x10000
				else
					maximum = tsize
				end
			else
				maximum = 0x10000
			end
		end
		
		pos = end_pos
	elseif id == 5 then -- memory
		print("section=memory")
		local end_pos = pos + slen
		pos = end_pos
	elseif id == 6 then -- global
		print("section=global")
		local end_pos = pos + slen
		pos = end_pos
	elseif id == 7 then -- export
		print("section=export")
		local end_pos = pos + slen
		
		local export_count = read_LEB(32, false)
		for e = 1, export_count do
			local name = readString()
			local external_kind = wasmFile[pos]
			pos = pos + 1
			local index = read_LEB(32, false)
			local eidx = m_export_count
			m_export_count = m_export_count + 1
			local exp = {external_kind, name, 0}
			exports[eidx + 1] = exp -- external kind, export name, value
			if external_kind == KIND_FUNCTION then
				exp[3] = functions[index + 1]
				--print("IDX " .. (index + 1))
				exp[3][5] = name
			elseif external_kind == KIND_TABLE then
				--
			elseif external_kind == KIND_MEMORY then
				--
			elseif external_kind == KIND_GLOBAL then
				exp[3] = globals[index + 1]
			end
			print("Export " .. name)
		end
		
		pos = end_pos
	elseif id == 8 then -- start
		print("section=start")
		local end_pos = pos + slen
		m_start_function = read_LEB(32, false)
		pos = end_pos
	elseif id == 9 then -- element
		print("section=element")
		local end_pos = pos + slen
		
		local elm_count = read_LEB(32, false)
		
		
		pos = end_pos
	elseif id == 10 then -- code
		print("section=code")
		local end_pos = pos + slen
		
		local functions_count = read_LEB(32, false)
		print("Function count: " .. tostring(functions_count))
		for i = 1, functions_count do
			local functionObj = functions[i + m_import_count]
			-- functionObj[3] = start, functionObj[4] = end
			--print(functionObj[1] .. " , " .. #functionObj[2])
			
			local size = read_LEB(32, false)
			local startOffset = pos
			-- func data: start at pos, end at pos + size
			local localBlockNum = read_LEB(32, false)
			local numLocals = 0
			local localVars = {0, 0}
			for l = 1, localBlockNum do
				local varCount = read_LEB(32, false)
				local wasmType = read_LEB(7, false) -- 127 is i32
				localVars[1] = varCount
				localVars[2] = wasmType
				numLocals = numLocals + varCount
				print("varCount: " .. tostring(varCount) .. ", wasmType: " .. tostring(wasmType))
			end
			local codeLen = (startOffset + size) - pos -- last remaining bytes
			
			
			local progStart = pos
			local progEnd = progStart + codeLen
			functionObj[3] = progStart
			functionObj[4] = progEnd

			--print(tableToString(functionObj))
			
			
			--[[local teststr = ""
			for x = pos, pos + codeLen - 1 do
				teststr = teststr .. wasmFile[x] .. ", "
			end
			print(teststr)]]
			local lastByte = wasmFile[pos + codeLen - 1]
			if lastByte ~= 0x0B then
				print("Function must end with 0x0B (end)")
			end
			
			--pos = pos + codeLen
			pos = startOffset + size
		end
		
		pos = end_pos
	elseif id == 11 then -- data
		print("section=data")
		local end_pos = pos + slen
		
		local seg_count = read_LEB(32, false)
		for s = 1, seg_count do
			local midx = read_LEB(32, false)
			
			local instruction = wasmFile[pos] -- i32.const?
			pos = pos + 1
			local iparam = read_LEB(32, false) -- parameter
			local iend = wasmFile[pos] -- end?
			pos = pos + 1
			print("ins " .. instruction .. ", par " .. iparam .. ", end " .. iend)
			
			local size = read_LEB(32, false)
			pos = pos + size
			print("Data idx " .. midx .. ", size " .. size .. ", offset " .. iparam)
		end
		
		pos = end_pos
	end
end

function funcInfo()
	for i = 1, #functions do
		local func = functions[i]
		print("------------------------")
		print("Index: " .. func[1])

		local params = "<None>"
		local returns = "<None>"

		local types = func[2]
		if #types[1] ~= 0 then
			params = ""
			for x = 1, #types[1] do
				params = params .. "," .. types[1][x]
			end
		end
		if #types[2] ~= 0 then
			returns = ""
			for x = 1, #types[2] do
				returns = returns .. "," .. types[2][x]
			end
		end

		print("Params: " .. params)
		print("Returns: " .. returns)
		print("Start: " .. func[3] .. "; End: " .. func[4])
		print("Name: " .. func[5])
		print("Is Imported: " .. tostring(func[6]))
		local imName = "<None>"
		if func[6] then
			imName = func[7] .. "." .. func[8]
		end
		print("Import name: " .. imName)
	end
end

local funcsByName = {}

for i = 1, #functions do
	local name = functions[i][5]
	if #name ~= 0 then
		funcsByName[name] = functions[i]
	end
end

runFunction(funcsByName.test)