# Clinical Webapp - Shiny Application

A modern, modular Shiny application for clinical trial management, recreated from Figma design specifications.

## 📁 Project Structure

```
clinical_webapp/
├── app.R                       # Main application entry point
├── global.R                    # Global configurations and constants
├── ui.R                        # User interface definition
├── server.R                    # Server-side logic
├── modules/                    # Modular components
│   ├── ui_components.R         # Reusable UI components (sidebar, header)
│   └── content_modules.R       # Content rendering for each module
├── www/                        # Static assets (images, CSS)
│   ├── styles.css              # Custom stylesheet
│   ├── Actalent.png            # Company logo
│   ├── Bell.png                # Notification bell icon
│   └── Profile_img.png         # User profile avatar
└── README.md                   # This file
```

## 🚀 Getting Started

### Prerequisites

- R (version 4.0 or higher)
- RStudio (recommended)
- Required R packages:
  - `shiny`
  - `htmltools`

### Installation

1. Install required packages:
```r
install.packages(c("shiny", "htmltools"))
```

2. Navigate to the project directory:
```r
setwd("path/to/clinical_webapp")
```

3. Run the application:
```r
shiny::runApp()
```

Or simply open `app.R` in RStudio and click "Run App".

## 📋 Features

### Navigation Modules

1. **Clinical** - Clinical trial management and workflows
2. **Data Management** - Electronic Data Capture and quality checks
3. **Blinded Biometrics** - Statistical analysis and biometric evaluation
4. **Medical Monitor** - Real-time dashboard with key metrics (default)
5. **RWE** - Real World Evidence collection and analysis
6. **DMC** - Data Monitoring Committee materials
7. **Client** - Client-facing reports and data access
8. **Admin** - System administration and user management

### Key Components

- **Sidebar Navigation**: Dark navy sidebar with logo and module buttons
- **Top Header**: Application title with notification bell and user profile
- **Main Content**: Dynamic content area that updates based on selected module
- **Responsive Design**: Adapts to different screen sizes

## 🎨 Design

The application is styled to exactly match the Figma design specifications:

- **Color Scheme**: 
  - Primary: `#0d1f36` (Dark Navy)
  - Background: `#e8ebf3` (Light Gray)
  - Text: `#0f172a` (Slate)
  
- **Typography**: Inter font family
- **Layout**: CSS Grid-based responsive layout

## 🏗️ Architecture

### Modular Design

The application follows a modular architecture for maintainability:

- **global.R**: Centralized configuration management
- **ui.R**: Main UI structure, imports UI components
- **server.R**: Server logic, routes to content modules
- **modules/ui_components.R**: Reusable UI functions (sidebar, header, buttons)
- **modules/content_modules.R**: Individual module content renderers

### Benefits

- ✅ Easy to maintain and update
- ✅ Clear separation of concerns
- ✅ Reusable components
- ✅ Well-documented code with comments
- ✅ Scalable for future enhancements

## 🔧 Customization

### Adding a New Module

1. Add menu item to `global.R`:
```r
list(id = "new_module", label = "New Module", active = FALSE)
```

2. Create content renderer in `modules/content_modules.R`:
```r
render_new_module <- function() {
  div(
    h2("New Module"),
    p("Module content here")
  )
}
```

3. Add route in `server.R`:
```r
"new_module" = render_new_module(),
```

### Styling Changes

Edit `www/styles.css` to customize colors, fonts, and layout.

### Asset Management

Place new images in the `www/` folder and reference them:
```r
tags$img(src = "image_name.png", alt = "Description")
```

## 📝 Code Style

The codebase follows these conventions:

- **Comments**: Section headers with equal signs for major sections
- **Function Docs**: `@description` and `@return` tags
- **Naming**: snake_case for functions and variables
- **Indentation**: 2 spaces
- **Organization**: Logical grouping with clear separators

## 🐛 Troubleshooting

### Images not loading
- Ensure images are in the `www/` folder
- Check file names match exactly (case-sensitive)
- Verify working directory is set to app root

### CSS not applied
- Clear browser cache
- Check for syntax errors in `styles.css`
- Verify CSS file path in `ui.R`

## 📄 License

Copyright © 2026 Actalent. All rights reserved.

## 👥 Support

For issues or questions, contact the development team.

---

**Version**: 1.0.0  
**Last Updated**: February 2026
