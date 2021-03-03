import sys
import os

print(os.system("git pull"))
print(os.system("git add ."))
print(os.system('git commit -m "'+sys.argv[1]+'"'))
print(os.system("git push"))

