import Vocabulary.vocabulary as voc
import re


pattern = re.compile(r'\s')

DB_FILE_PATH = './BAPI.db'
db = voc.DBControl()
Connect = db.getConnect(DB_FILE_PATH)
Cursor  = db.getCursor(Connect)
db.dropTable(Connect, 'BAPI')
create_table_sql = '''CREATE TABLE `BAPI` (
                          `id` int(16) NOT NULL,
                          `BAPI` varchar(256) NOT NULL,
                          `meaning` varchar(512) DEFAULT NULL,
                          `comment` varchar(512) DEFAULT NULL,
                           PRIMARY KEY (`id`)
                        )'''

db.createTable(Connect, create_table_sql)

try:
    fsock = open("./BAPI.txt", "r")
except IOError:
    print "The file don't exist, Please double check!"
    exit()
print 'The file mode is ',fsock.mode
print 'The file name is ',fsock.name

try:
    ist_of_all_the_lines = fsock.readlines()
    
    for line in ist_of_all_the_lines:
        
        "print line"
        BAPI_NAME = pattern.split(line)[0]
        print BAPI_NAME
        
finally:
    fsock.close()

fsock.close()

#check the file status
S1 = fsock.closed
if True == S1:
    print 'the file is closed'
else:
    print 'The file donot close'
