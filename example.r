
###################################################
################### LED EXAMPLE ###################
###################################################

###################################################
# The following LED example is modeled after Python Firmata
# Copyright (C) 2008  laboratorio (info@laboratorio.us)
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
###################################################


source("./firmata.r")

ard <- Arduino$new(port='/dev/tty.usbserial-A1001NQe')
init(ard)
con <- make_connection(ard, "test connection")

pin_mode(con, 13, OUTPUT)
delay(2)

while(True){
  digital_write(con, ard, 13, HIGH)
  delay(1)
  digital_write(con, ard, 13, LOW)
  delay(1)
}
