import os
import time
from datetime import datetime

current_date = time.strftime('%Y%m%d')
current_time = datetime.now().strftime("%H%M%S")

dest = os.getcwd()
# dest = dest + '\\testing\\'
dest = "/ufrc/djlemas/lewis.b/NLP/"
dest = dest + "\\breastfeeding_med_notes\\"
