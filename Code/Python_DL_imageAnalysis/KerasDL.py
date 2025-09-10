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
# SECTION 4: LOAD OTOLITH IMAGES FROM FOLDERS (FIXED VERSION)
# =============================================================================

print("\n" + "="*50)
print("LOADING OTOLITH IMAGES")
print("="*50)

# Get list of watershed folders and filter out hidden files
all_folders = os.listdir(data_path)
watershed_folders = [folder for folder in sorted(all_folders)
                     if os.path.isdir(os.path.join(data_path, folder))
                     and not folder.startswith('.')]

print(f"Found valid watershed folders: {watershed_folders}")

# Process each watershed folder with corrected indexing
# enumerate starts at 0!
for class_idx, watershed_folder in enumerate(watershed_folders):

    # Build full path to this watershed folder
    folder_path = os.path.join(data_path, watershed_folder)

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
                labels.append(class_idx)  # This will now be 0 for KK, 1 for NK
                image_count += 1

            except Exception as e:
                print(f"❌ Error loading {image_file}: {e}")

    print(f"   ✓ Loaded {image_count} images from {watershed_folder}")

# =============================================================================
# SECTION 5: DATA LOADING SUMMARY (UPDATED)
# =============================================================================

print("\n" + "="*50)
print("DATA LOADING COMPLETE")
print("="*50)
print(f"Total images loaded: {len(images)}")
print(f"Number of watersheds: {len(class_names)}")
print(f"Watershed classes: {class_names}")

# Show class mapping
print(f"\nClass mapping:")
for idx, name in enumerate(class_names):
    print(f"  Class {idx}: {name}")

# Show the shape of our data
if len(images) > 0:
    sample_image = images[0]
    print(f"\nImage data shape: {sample_image.shape}")
    print(f"  - Width: {sample_image.shape[1]} pixels")
    print(f"  - Height: {sample_image.shape[0]} pixels")
    print(f"  - Color channels: {sample_image.shape[2]} (RGB)")

# Show label distribution
unique_labels, counts = np.unique(labels, return_counts=True)
print(f"\nLabel distribution:")
for label, count in zip(unique_labels, counts):
    print(f"  Class {label} ({class_names[label]}): {count} images")

print("\n🎉 Ready for next step: Converting to arrays and splitting data!")

# =============================================================================
# SECTION 6: PREPARE DATA AND DEFINE INPUT SHAPE
# =============================================================================

print("\n" + "="*50)
print("PREPARING OTOLITH DATA FOR TRAINING")
print("="*50)

# Convert lists to numpy arrays
print("Converting image lists to numpy arrays...")
x_data = np.array(images)
y_data = np.array(labels)

print(f"✓ Images array shape: {x_data.shape}")
print(f"✓ Labels array shape: {y_data.shape}")

# Split into training and test sets
print("\nSplitting data into training and test sets...")
x_train, x_test, y_train, y_test = train_test_split(
    x_data, y_data,
    test_size=0.2,
    random_state=42,
    stratify=y_data
)

print(f"✓ Training set: {x_train.shape[0]} images")
print(f"✓ Test set: {x_test.shape[0]} images")

# Define input shape for the CNN model
input_shape = (128, 128, 3)  # Height, Width, Channels (RGB)
print(f"\n✓ Input shape for CNN: {input_shape}")

# Convert labels to categorical
num_classes = len(class_names)
y_train_categorical = keras.utils.to_categorical(y_train, num_classes)
y_test_categorical = keras.utils.to_categorical(y_test, num_classes)

print(f"✓ Number of classes: {num_classes}")
print(f"✓ Training labels shape: {y_train_categorical.shape}")
print(f"✓ Test labels shape: {y_test_categorical.shape}")

# =============================================================================
# SECTION 7: BUILD CNN MODEL FOR OTOLITH CLASSIFICATION
# =============================================================================

print("\n" + "="*50)
print("BUILDING CNN MODEL")
print("="*50)

# Build the CNN model with detailed explanations
model = keras.Sequential([

    # INPUT LAYER
    # Tells the model what size images to expect: 128x128 pixels with 3 color channels
    keras.Input(shape=input_shape),

    # =============================================================================
    # CNN BLOCK 1: FEATURE DETECTION
    # =============================================================================

    # CONVOLUTIONAL LAYER 1
    # Think of this as teaching the computer to recognize basic shapes and edges
    layers.Conv2D(
        filters=32,           # Creates 32 different "feature detectors"
        kernel_size=(3, 3),   # Each detector looks at 3x3 pixel areas
        activation="relu"     # ReLU = "if negative, make it 0; if positive, keep it"
    ),
    # What this does: Finds basic features like curves, edges, lines in otoliths

    # MAX POOLING LAYER 1
    # Think of this as "zooming out" - reduces image size while keeping important info
    layers.MaxPooling2D(pool_size=(2, 2)),
    # What this does: Takes each 2x2 area and keeps only the strongest signal
    # Reduces image from 128x128 to 64x64 pixels

    # =============================================================================
    # CNN BLOCK 2: COMPLEX FEATURE DETECTION
    # =============================================================================

    # CONVOLUTIONAL LAYER 2
    # Now looks for more complex patterns using the basic features from layer 1
    layers.Conv2D(
        filters=64,           # Creates 64 different "complex feature detectors"
        # Still looks at 3x3 areas, but now of processed features
        kernel_size=(3, 3),
        activation="relu"
    ),
    # What this does: Combines basic features to recognize otolith-specific shapes

    # MAX POOLING LAYER 2
    # Another "zoom out" step
    layers.MaxPooling2D(pool_size=(2, 2)),
    # What this does: Reduces image from 64x64 to 32x32 pixels
    # Now we have 64 feature maps, each 32x32 pixels

    # =============================================================================
    # DENSE BLOCK: CLASSIFICATION DECISION
    # =============================================================================

    # FLATTEN LAYER
    # Converts the 2D feature maps into a 1D list of numbers
    layers.Flatten(),
    # What this does: Takes 64 feature maps (32x32 each) = 65,536 numbers
    # Arranges them in a single long list for the final decision layer

    # DENSE (FULLY CONNECTED) LAYER
    # This is the "decision maker" - looks at all features and decides: KK or NK?
    layers.Dense(
        num_classes,          # 2 outputs: one for KK probability, one for NK probability
        activation="softmax"  # Softmax ensures the two probabilities add up to 100%
    ),
    # What this does: Outputs something like [0.85, 0.15] meaning 85% confident it's KK
])

print("✓ CNN model built successfully!")

# =============================================================================
# MODEL SUMMARY AND EXPLANATION
# =============================================================================

print("\n" + "="*50)
print("MODEL ARCHITECTURE SUMMARY")
print("="*50)

model.summary()

print("\n" + "="*50)
print("WHAT EACH LAYER DOES FOR OTOLITH CLASSIFICATION")
print("="*50)

print("🔍 INPUT LAYER:")
print("   - Receives otolith photos: 128x128 pixels, RGB color")
print("   - Like showing the computer a high-resolution otolith image")

print("\n🎯 CONV2D LAYER 1 (32 filters):")
print("   - Detects basic features: edges, curves, boundaries")
print("   - Each of 32 filters learns different basic patterns")
print("   - ReLU activation removes negative values (keeps only positive features)")

print("\n📉 MAXPOOLING2D LAYER 1:")
print("   - Reduces image size: 128x128 → 64x64")
print("   - Keeps strongest features, discards weak signals")
print("   - Makes model faster and focuses on important details")

print("\n🎯 CONV2D LAYER 2 (64 filters):")
print("   - Detects complex otolith shapes and patterns")
print("   - Combines basic features into meaningful otolith characteristics")
print("   - 64 filters = can recognize 64 different complex patterns")

print("\n📉 MAXPOOLING2D LAYER 2:")
print("   - Further reduces size: 64x64 → 32x32")
print("   - Now has compact representation of otolith features")

print("\n🔄 FLATTEN LAYER:")
print("   - Converts 2D feature maps to 1D list")
print("   - Prepares data for final classification decision")

print("\n🧠 DENSE LAYER (Softmax):")
print("   - Makes final decision: KK or NK watershed?")
print("   - Softmax ensures probabilities add to 100%")
print("   - Output example: [0.92, 0.08] = 92% confident it's KK")

print("\n" + "="*50)
print("MODEL READY FOR TRAINING!")
print("="*50)
