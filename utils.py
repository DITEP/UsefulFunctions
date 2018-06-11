import sys
import re
import os
import datetime as dt


class File_Reader():
	"""Wrapper class for simple standard file reader (txt, csv, tsv...) with in-built generator.
	
	:param file_name: path to file
	:param sep: field seperator, none by default.
	:param suppress_newlines: Deletes \n at the end of every line, True by default
	:param skiplines: number of lines to skip, 0 by default
	:param strip_chars_pattern: regular expression for pattern deleting, none by default
	:param encoding: file encoding, utf8 by default

	:example:

	#Init
	my_file = File_Reader("path/to/file")
	#Read line by line
	for line in my_file.iter():
		do_something(line)

	my_file2 = File_Reader("path/to/file")
	#Get all lines in a python list
	all_lines = my_file2.readlines()
	"""
	def __init__(self, file_name, sep = "", suppress_newlines = True, skiplines = 0, strip_chars_pattern = "", encoding = ""):
		self.file_name = file_name
		self.sep = sep
		self.suppress_newlines = suppress_newlines
		self.skiplines = skiplines
		self.strip_chars_pattern = strip_chars_pattern
		self.encoding = "utf-8"
		if encoding:
			self.encoding = encoding

	def char_strip(self,string, pattern):
		return re.sub(pattern, '', string)

	def iter(self):
		
		self.fp = open(self.file_name, encoding = self.encoding)
		self.line = self.fp.readline()

		for i in range(self.skiplines):
			self.line = self.fp.readline()

		while self.line:
			
			if self.suppress_newlines:
				self.line = self.line[:-1]

			if self.sep:
				self.line = self.line.split(self.sep)
				if self.strip_chars_pattern:
					for i in range(len(self.line)):
						self.line[i] = self.char_strip(self.line[i], self.strip_chars_pattern)
			
			if self.strip_chars_pattern and type(self.line)!=list:
				self.line = self.char_strip(self.line, self.strip_chars_pattern)

			yield self.line
			self.line = self.fp.readline()

		self.fp.close()

	def readlines(self):
		text = []
		for self.line in self.iter():
			text.append(self.line)
		return(text)


class Task_Follower():
	"""Mini barre de progression en pourcentage pour les executions linéaire longues.

	#initialiser avec le nombre d'opérations
	t = Task_Follower(count)
	#incrémenter l'avencement
	t.step()"""
	def __init__(self, taskcount, message = "Completion: "):
		self.taskcount = taskcount
		self.done = 0
		self.message = message
		self.gen = self.ini()

	def ini(self):
		return self.next()

	def step(self):
		sys.stdout.write(next(self.gen))
		sys.stdout.flush()

	def next(self):
		while self.done < self.taskcount+1:
			yield str(self.message + "%.2f \r" % (100*self.done/self.taskcount))
			self.done+=1
		while True:
			yield "Task Done\r"


def head(l, start = 0, stop = 5):
	"""Equivalent de l'outil linux head."""
	print(l[start:stop])


class File_Maker():
	'''Wrapper class for simple file writting with version control. Auto-renaming of files to keep old files.
	The last file will be tagged .latest (by default) et previous versions with a number (.1, .2, .3, etc).

	:param path: path to save file WITHOUT extension and NO tags. Supply the extension in parameters.
	:param data_stream: If not empty, will be written to the file with the save() method.
	:param format: define the field separator for save() method. By default "". Other supported formats are csv, tsv et "".
	:param extension: extension of save file. If none given, it will be infered with the path name or the format.
	:param encoding: file encoding.
	:param latest_string: Last version annotation tag. ".latest" by default.
	:param olddata_dir: If given, path to directory where current ".latest" version will be moved. Number tags
	will be added as needed.

	:example:

	from utils import File_Maker as FM

	data = [["This", "is"], "my", 'data']
	
	# Init
	# Moving and renaming old files is only done when save() or get_filepointer() are called.
	save_file = FM("../test", data_stream = data, extension = ".txt", olddata_dir = "OLD_DATA/") 

	# Writting data in ../test.latest.txt
	save_file.save()

	# Get the file pointer for further operations.	
	with save_file.get_filepointer() as fp:
		fp.write("adzaf")
		fp.write("sth")
		fp.write("rthter")
		fp.write("rthet")
		fp.close()
	# File pointer disapears when close() is called.

	'''
	def __init__(self, path, data_stream = "", format = "tsv", extension = "", encoding = "utf-8",
	latest_string = ".latest", olddata_dir = ""):
	
		self.path = path
		
		self.file_name = self.get_filename()

		self.extension = extension
		if not extension:
			self.extension = self.get_extension()
			if not self.extension:
				self.extension = "."+format

		self.set_savedir()
		self.olddata_dir = olddata_dir

		# self.replace_old = replace_old
		# self.version_control = version_control

		self.mode = "w"
		self.encoding = encoding

		if self.mode is not 'a' and self.mode is not 'w':
			print("Warning, file oppening mode is not supported.")

		
		self.fp = None
		self.data_stream = data_stream

		self.format = format

		self.format_dict = {
		"tsv":'\t',
		"csv":';',
		"":""
		}

		self.sep = self.format_dict[format]
		self.latest_string = latest_string


	def get_filename(self):
		name = (self.path.split("/")[-1])
		if '.' in name:
			name = ".".join(name.split('.')[0:-1])
		return name

	def get_extension(self):
		ext = self.path
		if ".." in ext:
			ext = self.path.split("..")[-1]

		if '.' in ext:
			ext = ext.split(".")[-1]
			return "."+ext
		return ""

	def set_datastream(self, data_stream):
		self.data_stream = data_stream

	def set_savedir(self):
		sd_path = self.path.split("/")[0:-1]
		if len(sd_path) is not 0:
			os.chdir("/".join(sd_path))

	def get_filepointer(self):
		if self.fp:
			return self.fp

		save_name = self.file_name+self.latest_string+self.extension

		if not self.olddata_dir:
			files = [f for f in os.listdir() if os.path.isfile(f)]

			if save_name in files:
				i = 1
				rename = save_name
				while rename in files:
					rename = self.file_name+"."+str(i)+self.extension
					i+=1
				os.rename(save_name, rename)

		else:
			dest_files = [f for f in os.listdir(self.olddata_dir) if os.path.isfile(os.path.join(self.olddata_dir,f))]
			local_files = [f for f in os.listdir() if os.path.isfile(f)]	

			if save_name in local_files:
					i = 1
					rename = self.file_name+"."+str(i)+self.extension
					while rename in dest_files:
						rename = self.file_name+"."+str(i)+self.extension
						i+=1
					new_path = os.path.join(self.olddata_dir,rename)
					os.rename(save_name, new_path)

		self.fp = open(save_name, self.mode, encoding = self.encoding)
		return self.fp


	def save(self):

		if not self.fp:
			self.fp = self.get_filepointer()

		if self.data_stream and self.fp:
			for i in self.data_stream:
				if type(i) is not str:
					self.fp.write(self.sep.join(i)+'\n')
				else:
					self.fp.write(i+'\n')

		else:
			sys.stdout.write("Missing data_stream or no file to write to.\n")
			sys.stdout.flush()
		

	def close(self):
		if self.fp:
			self.fp.close()
			self.fp = None
		else:
			sys.stdout.write("Can't close undefined file pointer.\n")
			sys.stdout.flush()