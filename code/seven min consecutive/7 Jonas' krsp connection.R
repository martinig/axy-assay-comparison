#consecutive seven minute sampling

#Connect to database - replace with your username below
#last updated on Feb 13, 2024 by A. R. Martinig

#JONAS's connection to database
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com", dbname ="krsp", user="jsanders", password = keyring::key_get("krsp") )