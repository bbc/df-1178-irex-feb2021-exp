
------------------------------------------------------------ Step 8 - Look at results ------------------------------------------------------------
--- Take table name specific to the experiment and make general for the analysis
SELECT * FROM dataforce_sandbox.vb_exp_1178_actions LIMIT 5;
SELECT * FROM dataforce_sandbox.vb_exp_1178_users LIMIT 5;

--- Make current exp table into generic name for ease
--- Starts/Watches and clicks
DROP TABLE IF EXISTS central_insights_sandbox.vb_exp_actions;
CREATE TABLE central_insights_sandbox.vb_exp_actions AS
SELECT * FROM dataforce_sandbox.vb_exp_1178_actions; -- this will need to be the name of the current experiment

SELECT * FROM central_insights_sandbox.vb_exp_actions LIMIT 10;

-- Impressions
--DROP TABLE IF EXISTS central_insights_sandbox.vb_exp_impr;
--CREATE TABLE central_insights_sandbox.vb_exp_impr AS
--SELECT * FROM central_insights_sandbox.vb_exp_sort_featured_binge_module_impressions ; -- this will need to be the name of the current experiment

-- All users -- this includes everyone who never viewed content
DROP TABLE IF EXISTS central_insights_sandbox.vb_exp_users;
CREATE TABLE central_insights_sandbox.vb_exp_users AS
SELECT * FROM dataforce_sandbox.vb_exp_1178_users; -- this will need to be the name of the current experiment
GRANT ALL on central_insights_sandbox.vb_exp_users to group central_insights_server;
DELETE FROM central_insights_sandbox.vb_exp_users WHERE age_range ISNULL;

---- Make sure anyone can use these tables
GRANT SELECT ON central_insights_sandbox.vb_exp_actions  TO GROUP dataforce_analysts;
--GRANT SELECT ON central_insights_sandbox.vb_exp_impr TO GROUP dataforce_analysts;
GRANT SELECT ON central_insights_sandbox.vb_exp_users TO GROUP dataforce_analysts;


------------------ Initial Checking ------------------
SELECt * FROM central_insights_sandbox.vb_exp_actions LIMIT 5;

SELECT DISTINCT dt FROM central_insights_sandbox.vb_exp_users;
SELECT age_range, count(distinct hashed_id) FROM central_insights_sandbox.vb_exp_users GROUP BY 1;

-- What is the split across different frequency bands -- a high number in the less frequent bands is bad for personalisation as we can't personalise them well
SELECT frequency_band, frequency_group_aggregated, count(DISTINCT dt||visit_id) as visits, count(distinct hashed_id) as signed_in_users
FROM central_insights_sandbox.vb_exp_users
GROUP BY 1,2
ORDER BY 1,2;

-- How many users in each group/platform
SELECT platform, exp_group, count(distinct hashed_id) AS num_users, count(distinct visit_id) AS visits
FROM central_insights_sandbox.vb_exp_users
GROUP BY 1,2;

SELECT DISTINCT click_container FROM central_insights_sandbox.vb_exp_actions;


------------------ Summary Numbers ------------------
---- hids, visits, starts, completes to the required module
with user_stats AS (
    -- get the number of users and visits for everyone in the experiment
    SELECT
        platform,
        exp_group,
        count(distinct hashed_id)                   as hids,
        count(distinct dt || hashed_id || visit_id) AS visits
    FROM central_insights_sandbox.vb_exp_users
    GROUP BY 1,2
),
     module_stats AS (
         -- Get the number of clicks and starts/watched from each module on homepage
         SELECT app_type,
                exp_group,
                count(visit_id)    AS module_clicks,
                sum(start_flag)    AS starts,
                sum(complete_flag) as completes

         FROM central_insights_sandbox.vb_exp_actions
         WHERE click_placement = 'home_page' --homepage
           AND (click_container = 'module-editorial-featured' or click_container = 'module-editorial-new-trending')
           --AND click_container = 'module-recommendations-recommended-for-you'
           --AND click_container = 'module-watching-continue-watching'
         GROUP BY 1, 2
     )
SELECT
    a.platform,
    a.exp_group,
    hids AS signed_in_users,
    visits,
    starts,
    completes,
    module_clicks
FROM user_stats a
         JOIN module_stats b ON a.exp_group = b.exp_group AND
        a.platform = b.app_type
ORDER BY a.platform,
         a.exp_group
;
---- hids, visits, starts, completes to the required module
with user_stats AS (
    -- get the number of users and visits for everyone in the experiment
    SELECT
        exp_group,
           CASE --set ages as needed for analysis
               WHEN age_range = '35-44' OR age_range = '45-54' or age_range = '55+' OR age_range = 'unknown' THEN '35+'
               ELSE age_range END as age_range,
        count(distinct hashed_id)                   as hids,
        count(distinct dt || hashed_id || visit_id) AS visits
    FROM central_insights_sandbox.vb_exp_users
    GROUP BY 1,2
),
     module_stats AS (
         -- Get the number of clicks and starts/watched from each module on homepage
         SELECT
                exp_group,
                CASE --set ages as needed for analysis
               WHEN age_range = '35-44' OR age_range = '45-54' or age_range = '55+' OR age_range = 'unknown' THEN '35+'
               ELSE age_range END as age_range,
                count(visit_id)    AS module_clicks,
                sum(start_flag)    AS starts,
                sum(complete_flag) as completes

         FROM central_insights_sandbox.vb_exp_actions
         WHERE click_placement = 'home_page' --homepage
           AND (click_container = 'module-editorial-featured' or click_container = 'module-editorial-new-trending')
           --AND click_container = 'module-recommendations-recommended-for-you'
           --AND click_container = 'module-watching-continue-watching'
         GROUP BY 1,2
     )
SELECT
    a.exp_group,
       a.age_range,
    hids AS signed_in_users,
    visits,
    starts,
    completes,
    module_clicks
FROM user_stats a
         LEFT JOIN module_stats b ON a.exp_group = b.exp_group and a.age_range = b.age_range
ORDER BY
         a.exp_group
;
------------------------------ Step 9: Data for R Statistical Analysis --------------------------------------------
-- Get data in right structure of stats analysis
/*
 1. select age groups needed
 2. select click container needed
 */
DROP TABLE IF EXISTS central_insights_sandbox.vb_exp_R_output;
CREATE TABLE central_insights_sandbox.vb_exp_R_output AS
with module_metrics AS ( --get starts/completes on modules
    SELECT exp_group,
           CASE --set ages as needed for analysis
               WHEN age_range = '35-44' OR age_range = '45-54' or age_range = '55+' OR age_range = 'unknown' THEN '35+'
               ELSE age_range END as age_range,
           hashed_id,
           sum(start_flag)        AS starts,
           sum(complete_flag)     as completes
    FROM central_insights_sandbox.vb_exp_actions
    WHERE (click_container = 'module-editorial-featured' or click_container = 'module-editorial-new-trending')-- module of interest
      AND click_placement = 'home_page'
    GROUP BY 1, 2, 3
),
     users AS (
         SELECT distinct exp_group,
                         CASE --set ages as needed for analysis
                             WHEN age_range = '35-44' OR age_range = '45-54' or age_range = '55+' OR
                                  age_range = 'unknown' THEN '35+'
                             ELSE age_range END as age_range
                 ,
                         hashed_id
         from central_insights_sandbox.vb_exp_users
     )
SELECT --row_number() over ()   as row_count,
       a.exp_group,
       a.age_range,
       a.hashed_id,
       isnull(b.starts, 0)    as starts,
       isnull(b.completes, 0) as completes
FROM users a
         LEFT JOIN module_metrics b on a.hashed_id = b.hashed_id and a.exp_group = b.exp_group
;
GRANT ALL on central_insights_sandbox.vb_exp_R_output to group central_insights_server;
DELETE FROM central_insights_sandbox.vb_exp_R_output WHERE age_range ISNULL;
SELECT min(row_count), max(row_count) FROM central_insights_sandbox.vb_exp_R_output limit 10;

SELECT age_range, count(distinct hashed_id) FROM central_insights_sandbox.vb_exp_R_output group by 1;
with user_count as (SELECT *, row_number() over (partition by hashed_id) as dup_count
                    FROM central_insights_sandbox.vb_exp_R_output)
SELECT dup_count, count(*)
from user_count
group by 1;
SELECT * FROM central_insights_sandbox.vb_exp_R_output;













--- Old


-------- Impressions to the specific module ---------
SELECT a.platform, exp_group, count(DISTINCT a.dt||a.visit_id) AS visits_saw_module
FROM central_insights_sandbox.vb_exp_impr a
         JOIN central_insights_sandbox.vb_exp_actions b
              ON a.dt = b.dt AND a.visit_id = b.visit_id AND a.platform = b.platform and a.hashed_id = b.hashed_id
WHERE container = 'module-editorial-featured'
GROUP BY 1, 2
ORDER BY 1,2
;

-------- Same as above summary but with age splits ---------
with user_stats AS (
    -- get the number of users and visits for everyone in the experiment
    SELECT platform,
           exp_group,
           age_range,
           count(distinct hashed_id)                   as hids,
           count(distinct unique_visitor_cookie_id)   as num_uv,
           count(distinct dt || hashed_id || visit_id) AS visits
    FROM central_insights_sandbox.vb_exp_users
    GROUP BY 1, 2, 3
),
     module_stats AS (
         -- Get the number of clicks and starts/watched from each module on homepage
         SELECT platform,
                exp_group,
                age_range,
                sum(start_flag)   AS starts,
                sum(complete_flag) as completes,
                count(visit_id)   AS module_clicks
         FROM central_insights_sandbox.vb_exp_actions
         WHERE click_placement = 'home_page' --homepage
           AND click_container = 'module-recommendations-recommended-for-you'
         GROUP BY 1, 2, 3
     )
SELECT a.platform,
       a.exp_group,
       a.age_range,
       hids AS signed_in_users,
       visits,
       starts,
       completes,
       module_clicks
FROM user_stats a
         JOIN module_stats b ON a.exp_group = b.exp_group
    AND a.platform = b.platform AND a.age_range = b.age_range
WHERE a.age_range != 'under 10'
ORDER BY a.platform,
         a.exp_group,
         a.age_range
;



---------- No platform split  --------------
SELECT exp_group,
       count(DISTINCT hashed_id) AS signed_in_users,
       count(DISTINCT dt||visit_id) AS visits
FROM central_insights_sandbox.vb_rec_exp_ids_hid
WHERE exp_group != 'unknown'
GROUP BY 1
ORDER BY 1;

SELECT exp_group,
       sum(start_flag)   as starts,
       sum(complete_flag) as completes,
       count(visit_id) AS num_clicks
FROM central_insights_sandbox.vb_exp_actions
WHERE click_container = 'module-recommendations-recommended-for-you'
AND click_placement = 'home_page' --homepage
GROUP BY 1
ORDER BY 1;



DROP TABLE IF EXISTS vb_rec_exp_results;
CREATE TEMP TABLE vb_rec_exp_results AS
with module_metrics AS (
    SELECT exp_group,
           age_range,
           hashed_id,
           sum(start_flag)   AS starts,
           sum(complete_flag) as completes
    FROM central_insights_sandbox.vb_exp_actions
    WHERE (click_container = 'module-editorial-featured' or click_container = 'module-editorial-new-trending')-- module of interest
      AND click_placement = 'home_page'
    GROUP BY 1, 2,3
)
SELECT DISTINCT a.exp_group,
                a.age_range,
                a.hashed_id,
                a.frequency_band,
                a.frequency_group_aggregated,
                ISNULL(b.starts, 0)  as starts,
                ISNULL(b.completes, 0) AS completes
FROM central_insights_sandbox.vb_rec_exp_ids_hid a -- get all users, even those who didn't click
         LEFT JOIN module_metrics b --Gives each user and their total starts/watched from that module
                   on a.hashed_id = b.hashed_id AND a.exp_group = b.exp_group AND a.age_range  = b.age_range
;

SELECT * FROM vb_rec_exp_results LIMIT 10;
-- Tables for R
--control
SELECT hashed_id, starts, completes FROM vb_rec_exp_results
WHERE exp_group = 'control'
--AND age_range = '35+'
;
--variation_1
SELECT hashed_id, starts, completes FROM vb_rec_exp_results
WHERE exp_group = 'variation_1'
--AND age_range = '35+'
;

------------------ For Test Duration Numbers ------------------
-- Only control group
-- Only one week
DROP TABLE IF EXISTS vb_exp_temp;
CREATE TEMP TABLE vb_exp_temp AS
with module_metrics AS (
    SELECT DISTINCT hashed_id,
           click_container,
           click_placement,
           sum(start_flag)   AS starts,
           sum(complete_flag) as completes
    FROM central_insights_sandbox.vb_exp_actions
    WHERE exp_group = 'control'
      AND dt BETWEEN 20200727 AND 20200802
    GROUP BY 1, 2, 3
),
     dist_users AS (
         SELECT DISTINCT hashed_id
         FROM central_insights_sandbox.vb_rec_exp_ids_hid
         WHERE exp_group = 'control'
           AND dt BETWEEN 20200727 AND 20200802
     )
SELECT DISTINCT b.click_container,
                b.click_placement,
                a.hashed_id,
                ISNULL(b.starts, 0)  as starts,
                ISNULL(b.completes, 0) AS completes
FROM dist_users a -- get all users, even those who didn't click
         LEFT JOIN module_metrics b --Gives each user and their total starts/watched from that module
                   on a.hashed_id = b.hashed_id
;
DROP TABLE vb_test;
CREATE TEMP TABLE vb_test AS
with
     -- compltes from featured rail
     featured AS
    (
    SELECT hashed_id, completes AS completes_featured
    FROM vb_exp_temp
    WHERE click_container = 'module-editorial-featured'
    AND click_placement = 'home_page'

),
     --completes from bingewothy rails
     binge AS (
    SELECT hashed_id, sum(completes) AS completes_binge
    FROM vb_exp_temp
    WHERE click_container ILIKE '%binge%'
         AND click_placement = 'home_page'
         GROUP BY 1

),
    homepage AS (
    SELECT DISTINCT hashed_id, sum(completes) AS completes_homepage
    FROM vb_exp_temp
        WHERE click_placement = 'home_page'
        GROUP BY 1
),
     dist_users AS (
         SELECT distinct hashed_id FROM vb_exp_temp
     )
SELECT  a.hashed_id,
       ISNULL(completes_featured,0) AS completes_featured,
       ISNULL(completes_binge,0) AS completes_binge,
       ISNULL(completes_homepage,0) AS completes_homepage
FROM dist_users a
LEFT JOIN featured b ON a.hashed_id = b.hashed_id
LEFT JOIN binge c ON a.hashed_id = c.hashed_id
LEFT JOIN homepage d ON a.hashed_id = d.hashed_id

;

SELECT * FROM vb_test;

--------- Think Analytics Groups -----------------------
with module_metrics AS (
    SELECT click_think_group,
           hashed_id,
           sum(start_flag)   AS starts,
           sum(complete_flag) as completes
    FROM central_insights_sandbox.vb_exp_actions
    WHERE click_container = 'module-recommendations-recommended-for-you'
      AND click_placement = 'home_page'
    GROUP BY 1, 2
),
     user_stats AS (
         SELECT DISTINCT a.hashed_id,
                         b.click_think_group,
                         ISNULL(b.starts, 0)  as starts,
                         ISNULL(b.completes, 0) AS completes
         FROM central_insights_sandbox.vb_rec_exp_ids_hid a
                  LEFT JOIN module_metrics b
                            on a.hashed_id = b.hashed_id)
SELECT click_think_group, count(hashed_id) as num_clicks, sum(starts) as starts, sum(completes) as completes
FROM user_stats
GROUP BY 1;
;

--------- How did the exp affect the whole product? ---------
with user_stats AS (
    -- get the number of users and visits for everyone in the experiment
    SELECT
        platform,
        exp_group,
        count(distinct hashed_id)                   as hids,
        count(distinct unique_visitor_cookie_id)   as num_uv,
        count(distinct dt || hashed_id || visit_id) AS visits
    FROM central_insights_sandbox.vb_exp_users
    GROUP BY 1,2
),
     module_stats AS (
         -- Get the number of clicks and starts/watched from each module on homepage
         SELECT
             platform,
             exp_group,
             sum(start_flag)   AS starts,
             sum(complete_flag) as completes,
             count(visit_id)   AS num_clicks
         FROM central_insights_sandbox.vb_exp_actions
         GROUP BY 1,2
     )
SELECT
    a.platform,
   a.exp_group,
    hids AS signed_in_users,
    visits,
    starts,
    completes,
    num_clicks
FROM user_stats a
         JOIN module_stats b ON a.exp_group = b.exp_group AND
        a.platform = b.platform
ORDER BY a.platform,
         a.exp_group
