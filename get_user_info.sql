
------------------------------------------------------ Step 1 - check the experiment ID and dates are correct. ------------------------------------------------------

SELECT distinct user_experience
FROM s3_audience.publisher
WHERE user_experience ILIKE '%EXP=iplxp_irex08_homepage07%'
  AND dt between 20210211 and 20210224;


------------------------------------------------------ Step 2 - create tables for date and exp group ------------------------------------------------------

--Date table
DROP TABLE IF EXISTS central_insights_sandbox.vb_exp_date_range;
create table central_insights_sandbox.vb_exp_date_range
(
    min_date varchar(20),
    max_date varchar(20)
);
insert into central_insights_sandbox.vb_exp_date_range
values ('20210211', '20210224');
SELECT *
FROM central_insights_sandbox.vb_exp_date_range;


------------------------------------------------------  Step 3 -  get the table of users ------------------------------------------------------
 -- you will need to set the variants and experiment name after line 56

DROP TABLE IF EXISTS dataforce_sandbox.vb_exp_1178_users;
CREATE TABLE dataforce_sandbox.vb_exp_1178_users AS
with get_age as (
    SELECT DISTINCT bbc_hid3,
                    age::integer           as age,
                    CASE
                        WHEN age::integer <= 13 THEN 'under 13'
                        WHEN age::integer between 14 and 15 THEN '14-16'
                        WHEN age::integer between 16 and 24 THEN '16-24'
                        WHEN age::integer between 25 and 34 THEN '25-34'
                        WHEN age::integer between 35 and 44 THEN '35-44'
                        WHEN age::integer between 45 and 54 THEN '45-54'
                        WHEN age::integer >= 55 THEN '55+'
                        ELSE 'unknown' END as age_range
    from prez.id_profile
    WHERE status != 'deleted'
),
     get_platform as (
         SELECT DISTINCT dt, app_type, visit_id
         FROM s3_audience.audience_activity
         WHERE dt between (SELECT min_date FROM central_insights_sandbox.vb_exp_date_range) and (SELECT max_date FROM central_insights_sandbox.vb_exp_date_range)
           AND destination = 'PS_IPLAYER'
           and app_type IS NOT NULL
     ),
     get_hid as (
         SELECT distinct dt, unique_visitor_cookie_id, visit_id, hashed_id
         FROM s3_audience.visits
         WHERE dt between (SELECT min_date FROM central_insights_sandbox.vb_exp_date_range) and (SELECT max_date FROM central_insights_sandbox.vb_exp_date_range)
           AND destination = 'PS_IPLAYER'
     ),
     exp_user as (
         SELECT distinct dt,
                         visit_id,
                         unique_visitor_cookie_id,
                         CASE
                             WHEN user_experience ILIKE '%control%' THEN 'control'
                             WHEN user_experience ILIKE '%variation_1%' THEN 'var1'
                             WHEN user_experience ILIKE '%variation_2%' THEN 'var2'
                             WHEN user_experience ILIKE '%variation_3%' THEN 'var3' END as exp_group
         FROM s3_audience.publisher
         WHERE user_experience ILIKE '%EXP=iplxp_irex08_homepage07%'
           AND dt between (SELECT min_date FROM central_insights_sandbox.vb_exp_date_range) and (SELECT max_date FROM central_insights_sandbox.vb_exp_date_range)
     )

SELECT c.app_type as platform, a.dt, a.visit_id, b.hashed_id, a.exp_group, d.age_range
FROM exp_user a
         LEFT JOIN get_hid b on a.dt = b.dt and a.visit_id = b.visit_id
         LEFT JOIN get_platform c on a.dt = c.dt and a.visit_id = c.visit_id
         LEFT JOIN get_age d on b.hashed_id = d.bbc_hid3
;
--Remove kids
DELETE
FROM dataforce_sandbox.vb_exp_1178_users
WHERE age_range = 'under 13'
   OR age_range = '14-16';

-- Check data
SELECT * FROM dataforce_sandbox.vb_exp_1178_users limit 19;
SELECT age_range, count(distinct hashed_id) as hids, count(distinct dt || visit_id) as visits
FROM dataforce_sandbox.vb_exp_1178_users
GROUP BY 1
ORDER BY 1;



------------------------------------------------------ Step 4 - Get the users' actions from the journey table ------------------------------------------------------

---Get all the user's behaviours
DROP TABLE IF EXISTS dataforce_sandbox.vb_exp_1178_actions;
CREATE TABLE dataforce_sandbox.vb_exp_1178_actions as
SELECT a.*, b.exp_group
FROM central_insights_sandbox.vb_journey_start_watch_complete a
         RIGHT JOIN dataforce_sandbox.vb_exp_1178_users b on a.dt = b.dt AND a.visit_id = b.visit_id
WHERE a.dt between (SELECT min_date FROM central_insights_sandbox.vb_exp_date_range) and (SELECT max_date FROM central_insights_sandbox.vb_exp_date_range)
;



