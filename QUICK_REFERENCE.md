# ==============================================================================
# CLINICAL WEBAPP - QUICK REFERENCE GUIDE
# ==============================================================================

## File Structure Overview

clinical_webapp/
│
├── app.R                          # ⚡ MAIN - Start here, launches the app
├── global.R                       # 🔧 CONFIG - Edit menu items and constants
├── ui.R                           # 🎨 UI - Overall page structure
├── server.R                       # ⚙️  SERVER - Business logic and routing
├── setup.R                        # 📦 SETUP - Run once to verify installation
│
├── modules/
│   ├── ui_components.R            # 🧩 UI building blocks (sidebar, header)
│   └── content_modules.R          # 📄 Page content for each menu item
│
├── www/
│   ├── styles.css                 # 💅 All visual styling
│   ├── Actalent.png               # Logo image
│   ├── Bell.png                   # Notification icon
│   └── Profile_img.png            # User avatar
│
├── README.md                      # 📖 Full documentation
└── .gitignore                     # 🚫 Git exclusions

## Quick Start

1. Open RStudio
2. Open app.R
3. Click "Run App" button
   OR
   Run: shiny::runApp()

## Common Tasks

### Change Menu Items
📝 Edit: global.R → MENU_ITEMS list

### Add New Page
1. 📝 Add menu in global.R
2. 📝 Create render function in modules/content_modules.R
3. 📝 Add route in server.R

### Update Colors/Styling
📝 Edit: www/styles.css

### Change Logo/Images
📝 Replace files in www/ folder

### Modify Header/Sidebar
📝 Edit: modules/ui_components.R

## Module Descriptions

| Module             | Function                           | File                      |
|--------------------|------------------------------------|---------------------------|
| Clinical           | Trial management workflows         | content_modules.R         |
| Data Management    | EDC and data quality               | content_modules.R         |
| Blinded Biometrics | Statistical analysis               | content_modules.R         |
| Medical Monitor    | Dashboard with metrics (DEFAULT)   | content_modules.R         |
| RWE                | Real-world evidence                | content_modules.R         |
| DMC                | Data Monitoring Committee          | content_modules.R         |
| Client             | Client reporting                   | content_modules.R         |
| Admin              | User and system management         | content_modules.R         |

## Key Variables (global.R)

- APP_TITLE: Application name displayed in header
- DEFAULT_MODULE: Which page loads first
- MENU_ITEMS: List of all navigation options

## CSS Classes (styles.css)

- .main-container: Overall grid layout
- .left-sidebar: Navigation panel
- .top-header: Title bar
- .main-content: Page content area
- .menu-item: Individual nav buttons
- .menu-item.active: Selected nav button

## Troubleshooting

❌ App won't start
   → Run setup.R first
   → Check all files present
   → Verify packages installed

❌ Images not showing
   → Check www/ folder
   → Verify file names exact
   → Check src= paths in code

❌ Styling looks wrong
   → Clear browser cache
   → Check styles.css syntax
   → Verify CSS link in ui.R

❌ Menu not working
   → Check JavaScript in ui.R
   → Verify onclick handlers
   → Check input$selected_menu in server.R

## Development Tips

✅ Always work in the clinical_webapp/ directory
✅ Test after each change
✅ Keep backups before major edits
✅ Follow existing code patterns
✅ Add comments for complex logic
✅ Use meaningful variable names

## Support

For questions or issues:
1. Check README.md for detailed docs
2. Review code comments
3. Check troubleshooting section above
