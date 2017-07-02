# How to add more test cases

1. Make a new folder named by a number
2. Run `python 1-rename.py` and input the new folder name.  This will rename files in the folder.
3. Run `python 2-make_answers.py` and input the new folder name.  Go through the prompt to input the correct answers to each image.  This will create a new json file named `ans-<folder>.json`.
4. Run `python 3-copy.py` and input the new folder name.  This will copy the files to the parent directory.
5. Done.



