CREATE TABLE "tbl_nebendiagnosen" (
	`id`	INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
	`patient_id`	INTEGER NOT NULL,
	`nebendiagnose`	TEXT,
	`timestamp`	INTEGER DEFAULT CURRENT_TIMESTAMP,
	FOREIGN KEY(`patient_id`) REFERENCES tbl_pedis ( id )
);
CREATE TABLE "tbl_endpoint" (
	`id`	INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
	`patient_id`	INTEGER NOT NULL,
	`outcome_endpoint_amputation`	TEXT,
	`amputation_height`	TEXT,
	`amputation_height_nonstandard`	TEXT,
	`endpoint_freetext`	TEXT,
	`behandlung_no_amputation`	TEXT,
	`timestamp`	TEXT DEFAULT CURRENT_TIMESTAMP,
	FOREIGN KEY(`patient_id`) REFERENCES tbl_pedis ( id )
);
CREATE TABLE "tbl_pedis" (
	`ID`	INTEGER PRIMARY KEY AUTOINCREMENT,
	`perfusion`	INTEGER,
	`extent`	INTEGER,
	`infection`	INTEGER,
	`depth`	INTEGER,
	`sensation`	INTEGER,
	`initial_therapy`	TEXT,
	`name`	TEXT,
	`admission_date`	date,
	`first_name`	TEXT,
	`birth_date`	DATE,
	`gender`	TEXT,
	`lokalisation`	INTEGER,
	`timestamp`	DATETIME DEFAULT CURRENT_TIMESTAMP
, dialyse text, crp numeric);
