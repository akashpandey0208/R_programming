1. Purpose:
   -> Connects to PostgreSQL
   -> Finds access_requests and panel_change_requests
   -> Marks requests as expired if:
   -> Status is pending
   -> requested_at/created_on is older than configured expiry days

2. configure .env file for DataBase connection and to set expiry days
   example: DB_HOST=localhost
   DB_PORT=5432
   DB_NAME=clinical-poc
   DB_USER=postgres
   DB_PASSWORD=your_password_here
   EXPIRY_DAYS=15

3. Install Dependencies
   -> step 1: create a virtual environment
   python3 -m venv venv
   source venv/bin/activate
   -> step 2: install dependenies
   pip install psycopg2-binary python-doten

4. To execute script manually
   -> From project root:
   source venv/bin/activate
   python scripts/expire_panel_change_requests.py
   Expected output:
   [2026-03-03 12:00:00] Expired rows: X

5. Schedule Cron job (linux)
   -> execute following commands and get the absolute path of Python and project.
   which python 3
   pwd
   -> Edit cronTab
   crontab -e
   -> add Cron entry (every 15 mins)
   _/15 _ \* \* \* /home/user/clinical-app/venv/bin/python /home/user/clinical-app/scripts/expiry_job.py >> /home/user/clinical-app/cron.log 2>&1

   here replace the python path and project path

   -> verify cron is running
   cat cron.log

6. Troubleshooting
   -> Use full absolute paths only
   -> Do NOT use python — use full path
   -> Ensure .env is in project root
   -> Ensure venv is activated in cron path
   -> Check permissions:
   chmod +x scripts/expire_panel_change_requests.py
