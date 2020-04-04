##############################################################################
# Import
import os
import getpass
import tkinter as tk

from tkinter.filedialog import askopenfilename
from cryptography.fernet import Fernet
from redcap import Project, RedcapError
##############################################################################
root = tk.Tk()
username = getpass.getuser()
rc_directory = "C:/Users/" + str(username) + "/BlueCrypt/"
rc_filename = "redcap_ehr.txt"
rc_filepath = rc_directory+rc_filename
rc_url = "https://redcap.ctsi.ufl.edu/redcap/api/"
key = b'R9ICI9P4gzDpZxZ-zTSuiYCX02TQpXt-J-jrnBasnJ0='
fer = Fernet(key)

# if you want to use the returned value of a function elsewhere
# (i.e. from genrkey()), then you need to assign the function a variable
# example = genrkey(); print(example) --> "whatever 'key' is"


def genrkey():

    root.withdraw()
    root.title("Select Container File")
    # container = askopenfilename(filetypes=(("PNG Files", "*.png"),
    #                                        ("All Files", "*.*")))

    key = Fernet.generate_key()
    return key

    # key needs to go into steganographic image


def encry(token_path=None, filename=rc_filename):

    if token_path is None:
        root.withdraw()
        token_path = askopenfilename(filetypes=(("Text Files", "*.txt"),
                                                ("All Files", "*.*")))

    with open(token_path, 'r') as wf:
        for line in wf:
            token = line.encode('ASCII')
        encr_token = fer.encrypt(token)

    if not os.path.exists(rc_directory):
        os.makedirs(rc_directory)
        with open(rc_directory+filename, "wb") as wf:
            wf.write(encr_token)
    else:
        with open(rc_directory+filename, "wb") as wf:
            wf.write(encr_token)

    os.remove(token_path)

    # pre-generated key is located in steganographic image
    # pull key from image


def decry(encr_token_path=rc_filepath):
    with open(encr_token_path, "rb") as wf:
        for line in wf:
            token = fer.decrypt(line).decode("ASCII")
        # print(token)
        return token
    # Need steganographic image with pre-generated key


def verify(url, filepath):
    token = decry(filepath)
    try:
        project = Project(url, token)
    except RedcapError:
        print("Please check that your API token is correct and that you are "
              "connected to the VPN")
        pass
    else:
        print("Valid API token and URL")


##############################################################################
# All code below this line is meant for debugging purposes only

if __name__ == "__main__":
    # root = tk.Tk()
    # root.withdraw()
    # fer = Fernet(key)
    # root.title("Select API token file")

    # api_path = askopenfilename(filetypes=(("Text Files", "*.txt"),
    #                                       ("All Files", "*.*")))
    encry()


# I want a histogram with x-axis separated my months, y-axis is frequency
# For each month, I want a bar for each year next to a 'total' bar for that
    # month (ideally with each bar per month being a separate color, but each
    # year being the same)

