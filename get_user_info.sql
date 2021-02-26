SELECT distinct user_experience
FROM s3_audience.publisher
WHERE user_experience ILIKE '%EXP=iplxp_irex08_homepage07%'
AND dt between 20210211 and 20210224;

DROP TABLE IF EXISTS dataforce_sandbox.vb_exp_1178_users;
CREATE TABLE dataforce_sandbox.vb_exp_1178_users AS
with get_platform as (
    SELECT dt, app_type, visit_id
    FROM s3_audience.audience_activity
    WHERE dt between 20210211 and 20210224
      AND destination = 'PS_IPLAYER'
),
     get_hid as (
         SELECT distinct dt, unique_visitor_cookie_id, visit_id, hashed_id
         FROM s3_audience.visits
         WHERE dt between 20210211 and 20210224
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
           AND dt between 20210211 and 20210224)

SELECT c.app_type as platform, a.dt, a.visit_id, b.hashed_id, a.exp_group
FROM exp_user a
         LEFT JOIN get_hid b on a.dt = b.dt and a.visit_id = b.visit_id
         LEFT JOIN get_platform c on a.dt = c.dt and a.visit_id = c.visit_id
;


SELECT * FROM dataforce_sandbox.vb_exp_1178_users limit 19;
SELECT exp_group, count(distinct hashed_id) as hids, count(distinct dt||visit_id) as visits
FROM dataforce_sandbox.vb_exp_1178_users
GROUP BY 1
ORDER BY 1;


---Get all the user's behaviours
CREATE TABLE dataforce_sandbox.vb_exp_1178_actions as
SELECT a.*, b.exp_group
FROM central_insights_sandbox.vb_journey_start_watch_complete a
RIGHT JOIN dataforce_sandbox.vb_exp_1178_users b on a.dt = b.dt AND a.visit_id = b.visit_id
AND a.dt between 20210211 and 20210224
;



--Impressions
SELECT * FROM central_insights_sandbox.vb_journey_homepage_impressions_summary LIMIT 10;