import psycopg2
import os
from datetime import datetime
from dotenv import load_dotenv

# Load .env file
load_dotenv()

# Read environment variables
DB_CONFIG = {
    "host": os.getenv("DB_HOST"),
    "port": int(os.getenv("DB_PORT")),
    "dbname": os.getenv("DB_NAME"),
    "user": os.getenv("DB_USER"),
    "password": os.getenv("DB_PASSWORD")
}

ACCESS_REQUEST_EXPIRY_DAYS = int(os.getenv("ACCESS_REQUEST_EXPIRY_DAYS"))
PANEL_CHANGE_EXPIRY_DAYS = int(os.getenv("PANEL_CHANGE_EXPIRY_DAYS"))
SYSTEM_USER = os.getenv("SYSTEM_USER")


def expire_access_requests(cursor):
    query = """
        UPDATE access_requests
        SET status = 'expired',
            last_modified_on = NOW(),
            last_modified_by = %s
        WHERE status = 'pending'
          AND created_on < NOW() - INTERVAL %s;
    """

    interval_value = f"{ACCESS_REQUEST_EXPIRY_DAYS} days"
    cursor.execute(query, (SYSTEM_USER, interval_value))
    return cursor.rowcount


def expire_panel_change_requests(cursor):
    query = """
        UPDATE panel_change_requests
        SET panel_request_status = 'expired',
            secondary_decision_at = NOW()
        WHERE panel_request_status = 'pending'
          AND requested_at < NOW() - INTERVAL %s;
    """

    interval_value = f"{PANEL_CHANGE_EXPIRY_DAYS} days"
    cursor.execute(query, (interval_value,))
    return cursor.rowcount


def run_expiry_jobs():
    conn = None
    try:
        conn = psycopg2.connect(**DB_CONFIG, connect_timeout=10)
        cursor = conn.cursor()

        access_expired = expire_access_requests(cursor)
        panel_expired = expire_panel_change_requests(cursor)

        conn.commit()

        print(f"[{datetime.now()}]")
        print(f"Access Requests expired: {access_expired}")
        print(f"Panel Change Requests expired: {panel_expired}")

    except Exception as e:
        print("Error:", e)
        if conn:
            conn.rollback()

    finally:
        if conn:
            cursor.close()
            conn.close()


if __name__ == "__main__":
    run_expiry_jobs()