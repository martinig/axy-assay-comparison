#Connect to database - replace with your username below
#original code by A. R. Martinig
#last edited April 16, 2024 by A. R. Martinig

#JONAS's connection to database
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com", dbname ="krsp", user="jsanders", password = keyring::key_get("krsp") )