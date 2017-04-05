library(methods)

# choose CRAN mirror of your choice
#install.packages("tcltk2", repos = "http://cran.us.r-project.org")
require(tcltk)
source("./serial/R/init.R")
source("./serial/R/listPorts.R")
source("./serial/R/pkgname.R")
source("./serial/R/serial.R")

# also need to work with "sleep" in time
# use Sys.sleep(seconds)

# to write to serial port...
# write.serialConnection(connection, message)
# to read...
# read.serialConnection(connection)


# constant values
DIGITAL_MESSAGE = 0x90 # send data for a digital port
ANALOG_MESSAGE = 0xE0 # send data for an analog pin (or PWM)
REPORT_ANALOG = 0xC0 # enable analog input by pin #
REPORT_DIGITAL = 0xD0 # enable digital input by port
SET_PIN_MODE = 0xF4 # set a pin to INPUT/OUTPUT/PWM/etc
REPORT_VERSION = 0xF9 # report firmware version
SYSTEM_RESET = 0xFF # reset from MIDI
START_SYSEX = 0xF0 # start a MIDI SysEx message
END_SYSEX = 0xF7 # end a MIDI SysEx message

# pin modes
INPUT = 0
OUTPUT = 1
PWM = 2
SERVO = 3

LOW = 0
HIGH = 1
MAX_DATA_BYTES = 32

# going to try to use a Reference class
# call methods by object$methodName
# access fields by object$property

Arduino <- setRefClass("Arudino",
                       fields = list(
                                port = "numeric",
                                wait = "numeric",
                                exec_multibyte_cmd ="numeric",
                                multibyte_channel = "numeric",
                                stored_input_data = "numeric",
                                parsing_sysex = "logical",
                                sysex_bytes_read = "numeric",
                                digital_output_data =  "numeric",
                                digital_input_data  = "numeric",
                                analog_input_data = "numeric",
                                major_version = "numeric",
                                minor_version = "numeric"
                                )
                       )


# start writing methods

str <- function(board){
  print("Arduino: ")
  print(board$port) 
}

# need to figure out how serial works in R
#pin_mode <- function(board, pin, mode){
  #board$serial
#}

# we will need to initialize all of the object variables
init <- function(board){
  board$wait = 0
  board$exec_multibyte_cmd = 0
  board$multibyte_channel = 0
  board$stored_input_data = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ,0, 0, 0)
  board$parsing_sysex = FALSE
  board$sysex_bytes_read = 0
  board$digital_output_data =  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  board$digital_input_data  = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  board$analog_input_data =  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) 
  board$major_version = 0
  board$minor_version = 0
}

set_version <- function(board, major, minor){
  board$major_version = major
  board$minor_version = minor
}

# open a serial connection independent of board
# board is where we will get all of our information
#print(class(serial))

make_connection <- function(board, con_name){

  serial = serialConnection(name=con_name, port=board$port, mode="115200,n,8,2")
  open(serial)
  return(serial)
}

pin_mode <- function(con, pin, mode){
  
  write.serialConnection(con, rawToChar(SET_PIN_MODE))
  write.serialConnection(con, rawToChar(pin))
  write.serialConnection(con, rawToChar(mode))
  print("executed")
}

digital_read <- function(con, board, pin){
  
  x = bitwAnd(pin, 0x07)
  first_shift = board$digital_input_data[bitwShiftR(pin, 3)]
  second_shift = bitwShiftR(first_shift, x)
  return(bitwAnd(second_shift, 0x01))
}

digital_write <- function(con, board, pin, value){

  port_number = bitwAnd(bitwShiftR(pin, 3), 0x0F)
  p = bitwAnd(pin, 0x07)

  if (value == 0){
    shifted = bitwShiftL(p, 1)
    board$digital_output_data[port_number] = bitwAnd(board$digital_output_data[port_number],
                                        bitwNot(shifted))
  }
  else{
    shifted = bitwShiftL(p, 1)
    board$digital_output_data[port_number] = bitwOr(board$digital_output_data[port_number],
                                        shifted)
  }
  write.serialConnection(con, rawToChar(bitwOr(DIGITAL_MESSAGE, port_number)))
  write.serialConnection(con, rawToChar(bitwAnd(board$digital_output_data[port_number],
                                                0x7F)))
  write.serialConnection(con, rawToChar(bitwShiftR(board$digital_output_data[port_number],
                                                7)))
}


analog_read <- function(board, pin){
  return(board$analog_input_data[pin])
}


analog_write <- function(con, board, pin, value){

  x = bitwAnd(pin, 0x0F)
  write.serialConnection(con, rawToChar(bitwOr(ANALOG_MESSAGE, x)))
  write.serialConnection(con, rawToChar(bitwAnd(value, 0x7F)))
  write.serialConnection(con, rawToChar(bitwShiftR(value, 7)))
  
}


set_version <- function(board, minor, major){
  board$major_version = major
  board$minor_version = minor
}


available <- function(con){
  return isOpen(con)
}

delay <- function(seconds){
  Sys.sleep(seconds)
}

parse <- function(board, con){
  data = read.serialConnection(con)
  if (data != ""){
    process(board, con, data) 
  }
}


process <- function(board, con, indata){

  command = NA
  if (board$parsing_sysex){
    if (indata == SYSEX_IN){ 
      board$parsing_sysex = FALSE
    } else{
      board$stored_input_data[board$sysex_bytes_read] = indata
      board$sysex_bytes_read = board$sysex_bytes_read + 1
    }
  } else if (board$wait_for_data > 0 & indata < 128){
    board$wait_for_data = board$wait_for_data - 1
    if (board$exec_multibyte_cmd != 0 & board$wait_for_data == 0){
      if (board$exec_multibyte_cmd == DIGITAL_MESSAGE){
        board$digital_input_data[board$multibyte_channel] = bitwShiftL(board$stored_input_data[0], 7) + board$stored_input_data[1] 
      } else if (board$exec_multibyte_cmd == ANALOG_MESSAGE){
        board$analog_input_data[board$multibyte_channel] = bitwShiftL(board$stored_input_data[0], 7) + board$stored_input_data[1] 
      } else if (board$exec_multibyte_cmd == REPORT_VERSION){
        set_version(board, board$stored_input_data[0], board$stored_input_data[1])
      } 
    } 
  } else{
      if (indata < 0xF0){
        command = bitwAnd(indata, 0xF0) 
        board$multibyte_channel = command
      } else {
          command = indata 
      }    
      if (command == DIGITAL MESSAGE or command == ANALOG_MSESAGE or command == REPORT_VERSION){
        board$wait_for_data = 2
        board$exec_multibyte_cmd = command
      }
  }
}


report <- function(con){

  delay(2)
  for (i in 0:5){
    write.serialConnection(con, rawToChar(bitwOr(REPORT_ANALOG, i))) 
    write.serialConnection(con, rawToChar(1))
  }

  for (i in 0:1){
    write.serialConnection(con, rawToChar(bitwOr(REPORT_DIGITAL, i))) 
    write.serialConnection(con, rawToChar(1))
  }
}


myard <- Arduino$new(port=7)
init(myard)
print(analog_read(myard, 8))







