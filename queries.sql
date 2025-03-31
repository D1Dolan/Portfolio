
-- customers who's name begin with j or c
SELECT first_name, last_name
FROM customer
WHERE customer_id  IN (SELECT CUSTOMER_customer_id FROM credit_card WHERE
card_type IN ('MasterCard','Visa')) AND first_name REGEXP '^J|^C|^c|^j'
LIMIT 5;

-- stores with more than 5 pharmacists 
SELECT store_id, COUNT(pharmacist_id) AS average_employees
FROM pharmacist JOIN store ON pharmacist.STORE_store_id = store.store_id GROUP BY store_id
HAVING average_employees > 5
LIMIT 5;

--Customers with loyalty points between 750 and 1000 and last name ends with h or b
SELECT first_name, last_name, loyalty_points
FROM customer
WHERE loyalty_points BETWEEN 750 AND 1000 AND last_name REGEXP '^H|^B|^h|^b'
LIMIT 5;


--Managers that are in stores that have open before 9am and close before 9pm
SELECT store_id, first_name, last_name
FROM store JOIN store_manager ON store.store_id=store_manager.STORE_store_id
WHERE store_start_time < '9:00 AM'  AND store_end_time < '9:00 PM'
ORDER BY first_name
LIMIT 5;


--Pharmacists that are in stores that have open after 11am and close before 5pm
SELECT store_id, first_name, last_name
FROM store JOIN pharmacist ON store.store_id=pharmacist.STORE_store_id
WHERE pharmacy_start_time > '11:00 AM'  AND pharmacy_end_time < '5:00 PM'
ORDER BY first_name
LIMIT 5;


--Average years of certification of all pharmacists of stores with pharmacists
SELECT store_id, ROUND(AVG(DATEDIFF(CURRENT_DATE,granted_date))/365,0) AS
average_years_since_certification
FROM store
LEFT JOIN pharmacist ON store.store_id=pharmacist.STORE_store_id
LEFT JOIN certification ON pharmacist.pharmacist_id =
certification.PHARMACIST_pharmacist_id
GROUP BY store_id
HAVING average_years_since_certification  IS NOT NULL
LIMIT 5;


--List the top 5 stores with the highest customer count.
SELECT store_id, COUNT(customer_id) AS customer_count
FROM store JOIN customer ON customer.STORE_store_id = store.store_id GROUP BY store_id
ORDER BY customer_count DESC
LIMIT 5;


--Calculate the average loyalty points of customers for each store.
SELECT store_id, AVG(loyalty_points) AS average_loyalty_points
FROM store JOIN customer ON customer.STORE_store_id = store.store_id
GROUP BY store_id
ORDER BY average_loyalty_points DESC
LIMIT 5;


--Calculate the average number of certifications per pharmacist in each store that has pharmacists.
SELECT store_id, FORMAT(AVG(number_of_certifications),0) AS
average_of_certifications_per_pharmacist
FROM store LEFT JOIN (
    SELECT STORE_store_id , pharmacist_id, COUNT(*) AS
number_of_certifications
    FROM certification LEFT JOIN pharmacist ON pharmacist.pharmacist_id =
certification.PHARMACIST_pharmacist_id
    GROUP BY STORE_store_id, pharmacist_id
) AS certification_count ON store.store_id =
certification_count.STORE_store_id
GROUP BY store_id
HAVING average_of_certifications_per_pharmacist IS NOT NULL LIMIT 5;


--Find the customers who have more loyalty points than the average loyalty points of all customers.
SELECT first_name, last_name, loyalty_points
FROM customer
WHERE loyalty_points > (SELECT AVG(loyalty_points) FROM customer)
ORDER BY loyalty_points
LIMIT 5;
