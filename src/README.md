->front_end/
Contains all UI-related R files.
auth/: UI for registration, login, registration summary, and password reset pages.
mainlayout/: UI files defining the main application layout and common page structure.

->controller/
Contains server-side logic corresponding to UI components.
auth/: Server logic for registration, login, summary, and reset functionalities.
mainlayout/: Server logic for handling the main layout behavior.

->main/
Contains app.R, the entry point required to run the Shiny application.

->libraries/
Stores all global library imports used across the application.

->components/
Reusable helper functions that are used repeatedly across UI and server logic.

->constants/
Holds application-wide constants and static configuration values.

->infrastructure/
Core backend integrations.
api/: API-related logic.
database/: Database connection and query logic.

->integrations/
Third-party service integrations (e.g., email.R for email services).

->operations/
Files for specific business or data operations (e.g., edctosmtpconversion.R).

->www/
Static assets for the UI.
style.css: All application CSS styles.
images/: Images used throughout the application.