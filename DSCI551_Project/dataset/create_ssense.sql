CREATE TABLE ssense.products (
	id int auto_increment NOT NULL,
	brand varchar(100) NOT NULL,
	description varchar(100)  NOT NULL,
	price_usd int NOT NULL,
	`type` varchar(100)  NOT NULL,
	CONSTRAINT `PRIMARY` PRIMARY KEY (id)
)
PARTITION BY KEY (id)
PARTITIONS 10;