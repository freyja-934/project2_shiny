def convert():
  fpr = open('temp/hourly_TEMP_2017.csv', 'r')
  fpw = open('temp/hourly_TEMP_2017_2.csv', 'w+b')
  line = fpr.readline()
  # Columns = State Code0	County Code1	Site Num2	Parameter Code3	POC4	
  # Latitude5	Longitude6	Datum7	Parameter Name8	Date Local9	Time Local10	
  # Date GMT11	Time GMT12	Sample Measurement13	Units of Measure14	MDL	Uncertainty15	Qualifier	Method16 Type17	Method Code18	Method Name19	
  # State Name20	County Name21	Date of Last Change22
  # split_line = line.split(',')
  # date = split_line[2].split(' ')[0]
  # time = split_line[2].split(' ')[1]
  # time = time.split('+')[0] a
  # print(date + 'T' + time + 'Z"')
  #fpw.write('State Code,County Code,Date Local,Time Local,Sample Measurement\n')
  i = 0
  while line:
    split_line = line.split(',')
    fpw.write(split_line[0] + ',' + split_line[1] + ',' + split_line[9] + ',' + split_line[10] + ',' + split_line[13]+'\n')
    line = fpr.readline()
    i+=1

  fpr.close()
  fpw.close()
  print(bytes(i))

convert()