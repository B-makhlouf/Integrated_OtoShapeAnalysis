# =============================================================================
# OTOLITH CLASSIFICATION USING KERAS DEEP LEARNING
# =============================================================================
# Purpose: Train a CNN to classify otolith photos by watershed based on shape
# Author: Your Name
# Date: September 2025
# =============================================================================

# =============================================================================
# SECTION 1: IMPORT LIBRARIES
# =============================================================================

# Core libraries for numerical operations and neural networks
import numpy as np                      # For numerical operations and arrays
import tensorflow as tf                 # TensorFlow main library
from tensorflow import keras            # Main deep learning framework
from tensorflow.keras import layers     # Neural network building blocks
import matplotlib.pyplot as plt        # For plotting and visualizing images

# Libraries for loading and organizing image files
import os                                           # For file system navigation
# Updated image loading tools
from tensorflow.keras.utils import load_img, img_to_array
# Data splitting tool
from sklearn.model_selection import train_test_split

print("✓ All libraries imported successfully!")

# =============================================================================
# SECTION 2: SET UP DATA PATHS AND PARAMETERS
# =============================================================================

# Path to your otolith image folders
data_path = "/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/ForOutlines/Original"

# Image size parameters - all photos will be resized to this standard size
img_height = 128    # Height in pixels
img_width = 128     # Width in pixels

print(f"✓ Data path set: {data_path}")
print(f"✓ Image dimensions: {img_width} x {img_height} pixels")

# =============================================================================
# SECTION 3: INITIALIZE DATA STORAGE
# =============================================================================

# Create empty lists to store our data
images = []         # Will hold all image pixel data as arrays
labels = []         # Will hold watershed numbers (0, 1, 2, etc.)
class_names = []    # Will hold watershed names ('KK', 'NK', etc.)

print("✓ Data storage initialized")

# =============================================================================
# SECTION 4: LOAD OTOLITH IMAGES FROM FOLDERS
# =============================================================================

print("\n" + "="*50)
print("LOADING OTOLITH IMAGES")
print("="*50)

# Get list of watershed folders
watershed_folders = sorted(os.listdir(data_path))
print(f"Found folders: {watershed_folders}")

# Process each watershed folder
for class_idx, watershed_folder in enumerate(watershed_folders):

    # Build full path to this watershed folder
    folder_path = os.path.join(data_path, watershed_folder)

    # Skip hidden files and non-directories
    if not os.path.isdir(folder_path) or watershed_folder.startswith('.'):
        print(f"⚠️  Skipping: {watershed_folder} (not a valid folder)")
        continue

    # Add this watershed to our class list
    class_names.append(watershed_folder)
    print(f"\n📁 Processing: {watershed_folder} (Class #{class_idx})")

    # Count images loaded from this watershed
    image_count = 0

    # Load all .jpg images from this watershed folder
    for image_file in os.listdir(folder_path):
        if image_file.lower().endswith('.jpg'):

            # Build complete path to image file
            img_path = os.path.join(folder_path, image_file)

            try:
                # Load and resize image to standard size
                img = load_img(img_path, target_size=(img_height, img_width))

                # Convert image to numerical array
                img_array = img_to_array(img)

                # Normalize pixel values to 0-1 range (helps neural network learn)
                img_array = img_array / 255.0

                # Store image data and its watershed label
                images.append(img_array)
                labels.append(class_idx)
                image_count += 1

            except Exception as e:
                print(f"❌ Error loading {image_file}: {e}")

    print(f"   ✓ Loaded {image_count} images from {watershed_folder}")

# =============================================================================
# SECTION 5: DATA LOADING SUMMARY
# =============================================================================

print("\n" + "="*50)
print("DATA LOADING COMPLETE")
print("="*50)
print(f"Total images loaded: {len(images)}")
print(f"Number of watersheds: {len(class_names)}")
print(f"Watershed classes: {class_names}")

# Show the shape of our data
if len(images) > 0:
    sample_image = images[0]
    print(f"Image data shape: {sample_image.shape}")
    print(f"  - Width: {sample_image.shape[1]} pixels")
    print(f"  - Height: {sample_image.shape[0]} pixels")
    print(f"  - Color channels: {sample_image.shape[2]} (RGB)")

print("\n🎉 Ready for next step: Converting to arrays and splitting data!")

# =============================================================================
# SECTION 6: PREPARE OTOLITH DATA FOR TRAINING (ADAPTED FROM MNIST EXAMPLE)
# =============================================================================

print("\n" + "="*50)
print("PREPARING OTOLITH DATA FOR TRAINING")
print("="*50)

# =============================================================================
# STEP 1: CONVERT TO NUMPY ARRAYS
# =============================================================================
# MNIST uses: (x_train, y_train), (x_test, y_test) = keras.datasets.mnist.load_data()
# For otoliths: We already loaded our data, now convert to numpy arrays

print("Converting image lists to numpy arrays...")
x_data = np.array(images)  # Convert our image list to numpy array
y_data = np.array(labels)  # Convert our label list to numpy array

print(f"✓ Images array shape: {x_data.shape}")
print(f"✓ Labels array shape: {y_data.shape}")
