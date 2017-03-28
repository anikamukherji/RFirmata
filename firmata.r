library(methods)
install.packages("serial", repos = "http://cran.us.r-project.org")
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
  board$serial = serialConnection(port=board$port, mode="115200,n,8,2")
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



myard <- Arduino$new(port=7)
init(myard)
str(myard)
