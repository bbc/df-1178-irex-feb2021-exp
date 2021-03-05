

 ------------------------------------------------------ Step 5 - use generic table a names and do some simple checks ------------------------------------------------------


--- Take table name specific to the experiment and make general for the analysis
SELECT * FROM dataforce_sandbox.vb_exp_1178_actions LIMIT 5;
SELECT * FROM dataforce_sandbox.vb_exp_1178_users LIMIT 5;

--- Make current exp table into generic name for ease
--- Starts/Watches and clicks
DROP TABLE IF EXISTS central_insights_sandbox.vb_exp_actions;
CREATE TABLE central_insights_sandbox.vb_exp_actions AS
SELECT * FROM dataforce_sandbox.vb_exp_1178_actions; -- this will need to be the name of the current experiment

SELECT * FROM central_insights_sandbox.vb_exp_actions LIMIT 10;

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
SELECT exp_group, count(distinct hashed_id) FROM central_insights_sandbox.vb_exp_users GROUP BY 1;

-- How many users in each group/platform
SELECT platform, exp_group, count(distinct hashed_id) AS num_users, count(distinct visit_id) AS visits
FROM central_insights_sandbox.vb_exp_users
GROUP BY 1,2;

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
           --AND (click_container = 'module-editorial-featured' or click_container = 'module-editorial-new-trending')
           AND click_container ILIKE '%binge%'
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

------------------------------------------------------ Step 6 - get summary .csv files for R analysis------------------------------------------------------------------------

-- Get data in right structure of stats analysis
/*
 1. Select age groups needed in case when statements
 2. Select click container needed
 3. Select click placement
 */

----- Step 6a. - Per browser consumption data -----
DROP TABLE IF EXISTS central_insights_sandbox.vb_exp_R_output;
CREATE TABLE central_insights_sandbox.vb_exp_R_output AS
with module_metrics AS ( --get starts/completes on modules
    SELECT exp_group,
           CASE --set ages as needed for analysis
               WHEN age_range = '35-44' OR age_range = '45-54' or age_range = '55+' OR age_range = 'unknown' THEN '35+'
               ELSE age_range END as age_range,
           hashed_id,
           count(*) as clicks,
           sum(start_flag)        AS starts,
           sum(complete_flag)     as completes
    FROM central_insights_sandbox.vb_exp_actions
    --WHERE click_placement = 'home_page'
    --AND click_container ILIKE '%binge%'
        -- AND (click_container = 'module-editorial-featured' or click_container = 'module-editorial-new-trending')-- module of interest
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
       isnull(b.clicks, 0) as clicks,
       isnull(b.starts, 0)  as starts,
       isnull(b.completes, 0) as completes
FROM users a
         LEFT JOIN module_metrics b on a.hashed_id = b.hashed_id and a.exp_group = b.exp_group
;
GRANT ALL on central_insights_sandbox.vb_exp_R_output to group central_insights_server;
DELETE FROM central_insights_sandbox.vb_exp_R_output WHERE age_range ISNULL;



SELECT * FROM central_insights_sandbox.vb_exp_R_output; -- Save this to your R project folder




----- Step 6b. - Hids and visits summary data -----
--Check age groups are as required.
--Save to r project

--- hids_visits_summary
SELECT CASE --set ages as needed for analysis
           WHEN age_range = '35-44' OR age_range = '45-54' or age_range = '55+' OR age_range = 'unknown' THEN '35+'
           ELSE age_range END                      as age_range,
       exp_group,
       count(distinct hashed_id)                as hids,
       count(distinct dt || hashed_id || visit_id) AS visits
FROM central_insights_sandbox.vb_exp_users
GROUP BY 1, 2;


----- Step 6c. - User frequency segments -----

---Set up frequncy information
DROP TABLE IF EXISTS central_insights_sandbox.vb_exp_freq_groups;
CREATE TABLE central_insights_sandbox.vb_exp_freq_groups as
with segment as (
    SELECT DISTINCT date_of_segmentation,
                    bbc_hid3,
                    age_range,
                    frequency_band,
                    avg_days_between_visits,
                    'iplayer' AS app_name
    FROM iplayer_sandbox.iplayer_weekly_frequency_calculations
    WHERE date_of_segmentation >= (SELECT trunc(date_trunc('week', cast(min(dt) as date)))
                                   FROM central_insights_sandbox.vb_exp_users)
      AND date_of_segmentation <= (SELECT trunc(date_trunc('week', cast(max(dt) as date)))
                                   FROM central_insights_sandbox.vb_exp_users)
),
     genders as (SELECT distinct bbc_hid3, gender
        FROM prez.id_profile WHERE status != 'deleted'),

     exp_seg_user as (
         SELECt a.*,
                c.gender,
                b.date_of_segmentation,
                case when b.frequency_band is null then 'new' else b.frequency_band end   as frequency_band,
                central_insights_sandbox.udf_dataforce_frequency_groups(b.frequency_band) as frequency_group_aggregated
         FROM central_insights_sandbox.vb_exp_users a
                  LEFT JOIN segment b on a.hashed_id = b.bbc_hid3 AND
                                         trunc(date_trunc('week', cast(a.dt as date))) = b.date_of_segmentation
         LEFT JOIN genders c on a.hashed_id = c.bbc_hid3
         ORDER BY hashed_id, dt
     )
SELECT distinct hashed_id, exp_group, age_range, frequency_band, frequency_group_aggregated, gender
FROM exp_seg_user
;

-- Check
SELECT exp_group, age_range, frequency_band, frequency_group_aggregated, gender, count(distinct hashed_id) as hids
FROM central_insights_sandbox.vb_exp_freq_groups
group by 1,2,3,4,5;

-- Get simple metrics for each frequency group
-- Save to r project
with module_metrics AS ( --get starts/completes on modules
    SELECT exp_group,
           CASE --set ages as needed for analysis
               WHEN age_range = '35-44' OR age_range = '45-54' or age_range = '55+' OR age_range = 'unknown' THEN '35+'
               ELSE age_range END as age_range,
           hashed_id,
           count(distinct dt||visit_id) as visits,
           count(*)               as clicks,
           sum(start_flag)        AS starts,
           sum(complete_flag)     as completes
    FROM central_insights_sandbox.vb_exp_actions
    WHERE click_placement = 'home_page'
      --click_container ILIKE '%binge%'
      --AND (click_container = 'module-editorial-featured' or click_container = 'module-editorial-new-trending')-- module of interest
    GROUP BY 1, 2, 3),
     join_tabs as (
         SELECT a.*, b.clicks, b.starts, b.completes, b.visits
         FROM central_insights_sandbox.vb_exp_freq_groups a
                  LEFT JOIN module_metrics b on a.hashed_id = b.hashed_id and a.exp_group = b.exp_group
     )
SELECT exp_group,
       frequency_group_aggregated,
       frequency_band,
       count(distinct hashed_id) as hids,
       sum(visits) as visits,
       sum(clicks)    as clicks,
       sum(starts)    as starts,
       sum(completes) as completes
FROM join_tabs
GROUP BY 1, 2,3
ORDER BY 1, 2 desc;








