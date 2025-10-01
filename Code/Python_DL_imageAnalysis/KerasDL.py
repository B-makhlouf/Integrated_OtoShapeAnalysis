# =============================================================================
# OTOLITH CLASSIFICATION USING KERAS DEEP LEARNING
# =============================================================================

import seaborn as sns
from sklearn.metrics import confusion_matrix, classification_report
import numpy as np
import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import layers
from tensorflow.keras.utils import load_img, img_to_array
import matplotlib.pyplot as plt
import os
from tensorflow.keras.utils import load_img, img_to_array
from sklearn.model_selection import train_test_split

print("✓ All libraries imported successfully!")

# =============================================================================
# SETUP
# =============================================================================

data_path = "/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Data/OtoPhotos/Original"
figures_path = "/Users/benjaminmakhlouf/Research_repos/Integrated_OtoShapeAnalysis/Figures/Deep Learning"
os.makedirs(figures_path, exist_ok=True)

img_height = 128
img_width = 128

images = []
labels = []
class_names = []

# =============================================================================
# LOAD IMAGES
# =============================================================================

all_folders = os.listdir(data_path)
watershed_folders = [folder for folder in sorted(all_folders)
                     if os.path.isdir(os.path.join(data_path, folder))
                     and not folder.startswith('.')]

print(f"Found valid watershed folders: {watershed_folders}")

for class_idx, watershed_folder in enumerate(watershed_folders):
    folder_path = os.path.join(data_path, watershed_folder)
    class_names.append(watershed_folder)
    print(f"Processing: {watershed_folder} (Class #{class_idx})")

    image_count = 0
    for image_file in os.listdir(folder_path):
        if image_file.lower().endswith('.jpg'):
            img_path = os.path.join(folder_path, image_file)
            try:
                img = load_img(img_path, target_size=(
                    img_height, img_width), color_mode='grayscale')
                img_array = img_to_array(img)
                img_array = img_array / 255.0
                images.append(img_array)
                labels.append(class_idx)
                image_count += 1
            except Exception as e:
                print(f"Error loading {image_file}: {e}")

    print(f"Loaded {image_count} images from {watershed_folder}")

print(f"Total images loaded: {len(images)}")
print(f"Watershed classes: {class_names}")

# =============================================================================
# IMAGE STATISTICS ANALYSIS - CHECK FOR LIGHTING BIAS
# =============================================================================

print("\n" + "="*80)
print("IMAGE STATISTICS ANALYSIS - DETECTING POTENTIAL LIGHTING BIAS")
print("="*80)

x_data = np.array(images)
y_data = np.array(labels)

# Calculate statistics for each watershed
watershed_stats = {}
for watershed_idx, watershed_name in enumerate(class_names):
    watershed_images = x_data[y_data == watershed_idx]

    mean_brightness = np.mean(watershed_images)
    std_brightness = np.std(watershed_images)
    median_brightness = np.median(watershed_images)
    min_brightness = np.min(watershed_images)
    max_brightness = np.max(watershed_images)

    # Calculate per-image statistics
    per_image_means = np.mean(watershed_images, axis=(1, 2, 3))
    per_image_stds = np.std(watershed_images, axis=(1, 2, 3))

    watershed_stats[watershed_name] = {
        'mean': mean_brightness,
        'std': std_brightness,
        'median': median_brightness,
        'min': min_brightness,
        'max': max_brightness,
        'per_image_means': per_image_means,
        'per_image_stds': per_image_stds,
        'n_images': len(watershed_images)
    }

    print(f"\n{watershed_name} (n={len(watershed_images)} images):")
    print(f"  Mean Brightness:   {mean_brightness:.4f}")
    print(f"  Std Brightness:    {std_brightness:.4f}")
    print(f"  Median Brightness: {median_brightness:.4f}")
    print(f"  Range:             [{min_brightness:.4f}, {max_brightness:.4f}]")
    print(f"  Avg per-image std: {np.mean(per_image_stds):.4f}")

# Calculate overall statistics for comparison
all_means = [stats['mean'] for stats in watershed_stats.values()]
mean_of_means = np.mean(all_means)
std_of_means = np.std(all_means)
cv_of_means = std_of_means / mean_of_means if mean_of_means > 0 else 0

print(f"\n{'='*80}")
print(f"OVERALL SUMMARY:")
print(f"  Mean of watershed means: {mean_of_means:.4f}")
print(f"  Std of watershed means:  {std_of_means:.4f}")
print(f"  Coefficient of variation: {cv_of_means:.4f}")

if cv_of_means > 0.10:
    print(
        f"\n⚠️  WARNING: High variation in brightness across watersheds (CV={cv_of_means:.2%})")
    print(f"    This suggests potential lighting bias that the model may exploit.")
elif cv_of_means > 0.05:
    print(
        f"\n⚡ CAUTION: Moderate variation in brightness across watersheds (CV={cv_of_means:.2%})")
    print(f"    Consider applying normalization or histogram equalization.")
else:
    print(
        f"\n✓ Good: Low variation in brightness across watersheds (CV={cv_of_means:.2%})")

# =============================================================================
# VISUALIZE BRIGHTNESS DISTRIBUTIONS
# =============================================================================

fig, axes = plt.subplots(2, 2, figsize=(15, 12))

# 1. Box plot of mean brightness by watershed
ax1 = axes[0, 0]
means_by_watershed = [watershed_stats[name]['per_image_means']
                      for name in class_names]
bp = ax1.boxplot(means_by_watershed, labels=class_names, patch_artist=True)
for patch, color in zip(bp['boxes'], plt.cm.Set3(range(len(class_names)))):
    patch.set_facecolor(color)
ax1.set_xlabel('Watershed', fontweight='bold')
ax1.set_ylabel('Mean Image Brightness', fontweight='bold')
ax1.set_title('Distribution of Mean Brightness by Watershed',
              fontsize=12, fontweight='bold')
ax1.grid(True, alpha=0.3, axis='y')
plt.setp(ax1.xaxis.get_majorticklabels(), rotation=45, ha='right')

# 2. Bar plot of mean brightness with error bars
ax2 = axes[0, 1]
means = [watershed_stats[name]['mean'] for name in class_names]
stds = [watershed_stats[name]['std'] for name in class_names]
x_pos = np.arange(len(class_names))
bars = ax2.bar(x_pos, means, yerr=stds, capsize=5,
               color=plt.cm.Set3(range(len(class_names))),
               edgecolor='black', alpha=0.7)
ax2.set_xlabel('Watershed', fontweight='bold')
ax2.set_ylabel('Mean Brightness ± Std', fontweight='bold')
ax2.set_title('Mean Brightness by Watershed (with Standard Deviation)',
              fontsize=12, fontweight='bold')
ax2.set_xticks(x_pos)
ax2.set_xticklabels(class_names, rotation=45, ha='right')
ax2.grid(True, alpha=0.3, axis='y')
ax2.axhline(y=mean_of_means, color='red', linestyle='--',
            label=f'Overall Mean: {mean_of_means:.3f}')
ax2.legend()

# 3. Histogram of all brightness values by watershed
ax3 = axes[1, 0]
for watershed_idx, watershed_name in enumerate(class_names):
    watershed_images = x_data[y_data == watershed_idx].flatten()
    ax3.hist(watershed_images, bins=50, alpha=0.5,
             label=watershed_name, density=True)
ax3.set_xlabel('Pixel Brightness Value', fontweight='bold')
ax3.set_ylabel('Density', fontweight='bold')
ax3.set_title('Distribution of All Pixel Values by Watershed',
              fontsize=12, fontweight='bold')
ax3.legend()
ax3.grid(True, alpha=0.3)

# 4. Scatter plot: mean vs std for each image
ax4 = axes[1, 1]
for watershed_idx, watershed_name in enumerate(class_names):
    per_img_means = watershed_stats[watershed_name]['per_image_means']
    per_img_stds = watershed_stats[watershed_name]['per_image_stds']
    ax4.scatter(per_img_means, per_img_stds, alpha=0.6, s=50,
                label=watershed_name)
ax4.set_xlabel('Per-Image Mean Brightness', fontweight='bold')
ax4.set_ylabel('Per-Image Std Brightness', fontweight='bold')
ax4.set_title('Image Brightness Characteristics by Watershed',
              fontsize=12, fontweight='bold')
ax4.legend()
ax4.grid(True, alpha=0.3)

plt.tight_layout()
plt.savefig(os.path.join(figures_path, 'brightness_analysis.png'),
            dpi=300, bbox_inches='tight')
plt.close()

print(
    f"\n✓ Brightness analysis plots saved to: {os.path.join(figures_path, 'brightness_analysis.png')}")
print("="*80 + "\n")

# =============================================================================
# GLARE/OVEREXPOSURE ANALYSIS
# =============================================================================

print("\n" + "="*80)
print("GLARE AND OVEREXPOSURE ANALYSIS")
print("="*80)

overexposure_stats = {}

for watershed_idx, watershed_name in enumerate(class_names):
    watershed_images = x_data[y_data == watershed_idx]

    # Detect overexposed pixels (very bright pixels)
    overexposed_threshold = 0.95  # Pixels brighter than 95%
    overexposed_pixels_per_image = np.mean(
        watershed_images > overexposed_threshold, axis=(1, 2, 3))

    avg_overexposure = np.mean(overexposed_pixels_per_image) * 100
    max_overexposure = np.max(overexposed_pixels_per_image) * 100
    std_overexposure = np.std(overexposed_pixels_per_image) * 100

    # Count images with significant overexposure
    significantly_overexposed = np.sum(
        overexposed_pixels_per_image > 0.10)  # >10% of pixels blown out
    moderately_overexposed = np.sum(
        overexposed_pixels_per_image > 0.05)  # >5% of pixels blown out

    overexposure_stats[watershed_name] = {
        'avg': avg_overexposure,
        'max': max_overexposure,
        'std': std_overexposure,
        'per_image': overexposed_pixels_per_image * 100,
        'n_significant': significantly_overexposed,
        'n_moderate': moderately_overexposed,
        'total_images': len(watershed_images)
    }

    print(f"\n{watershed_name} (n={len(watershed_images)} images):")
    print(f"  Avg overexposed pixels: {avg_overexposure:.2f}%")
    print(f"  Max overexposed pixels: {max_overexposure:.2f}%")
    print(f"  Std overexposed pixels: {std_overexposure:.2f}%")
    print(
        f"  Images with >10% overexposure: {significantly_overexposed}/{len(watershed_images)} ({significantly_overexposed/len(watershed_images)*100:.1f}%)")
    print(
        f"  Images with >5% overexposure:  {moderately_overexposed}/{len(watershed_images)} ({moderately_overexposed/len(watershed_images)*100:.1f}%)")

# Calculate variation in overexposure across watersheds
avg_overexposures = [stats['avg'] for stats in overexposure_stats.values()]
mean_of_overexposures = np.mean(avg_overexposures)
std_of_overexposures = np.std(avg_overexposures)

print(f"\n{'='*80}")
print(f"OVEREXPOSURE SUMMARY:")
print(f"  Mean overexposure across watersheds: {mean_of_overexposures:.2f}%")
print(f"  Std of overexposure across watersheds: {std_of_overexposures:.2f}%")

if std_of_overexposures > 2.0:
    print(f"\n⚠️  WARNING: High variation in overexposure across watersheds!")
    print(
        f"    Std = {std_of_overexposures:.2f}% - Model may use glare patterns to classify.")
elif std_of_overexposures > 1.0:
    print(
        f"\n⚡ CAUTION: Moderate variation in overexposure (Std={std_of_overexposures:.2f}%)")
else:
    print(
        f"\n✓ Low variation in overexposure across watersheds (Std={std_of_overexposures:.2f}%)")

if mean_of_overexposures > 10.0:
    print(
        f"\n⚠️  WARNING: High overall overexposure ({mean_of_overexposures:.1f}% avg)")
    print(f"    Consider excluding heavily overexposed images or clipping bright values.")

# Visualize overexposure distribution
fig, axes = plt.subplots(2, 2, figsize=(15, 12))

# 1. Histogram of overexposure by watershed
ax1 = axes[0, 0]
for watershed_name in class_names:
    overexposed_pct = overexposure_stats[watershed_name]['per_image']
    ax1.hist(overexposed_pct, bins=30, alpha=0.6,
             label=watershed_name, edgecolor='black')

ax1.set_xlabel('Percentage of Overexposed Pixels per Image', fontweight='bold')
ax1.set_ylabel('Number of Images', fontweight='bold')
ax1.set_title('Distribution of Overexposure by Watershed',
              fontsize=12, fontweight='bold')
ax1.legend()
ax1.grid(True, alpha=0.3)
ax1.axvline(x=5, color='orange', linestyle='--',
            linewidth=2, label='5% threshold')
ax1.axvline(x=10, color='red', linestyle='--',
            linewidth=2, label='10% threshold')

# 2. Box plot of overexposure by watershed
ax2 = axes[0, 1]
overexp_by_watershed = [overexposure_stats[name]
                        ['per_image'] for name in class_names]
bp = ax2.boxplot(overexp_by_watershed, labels=class_names, patch_artist=True)
for patch, color in zip(bp['boxes'], plt.cm.Set3(range(len(class_names)))):
    patch.set_facecolor(color)
ax2.set_xlabel('Watershed', fontweight='bold')
ax2.set_ylabel('Overexposed Pixels (%)', fontweight='bold')
ax2.set_title('Overexposure Distribution by Watershed',
              fontsize=12, fontweight='bold')
ax2.grid(True, alpha=0.3, axis='y')
ax2.axhline(y=5, color='orange', linestyle='--', alpha=0.7)
ax2.axhline(y=10, color='red', linestyle='--', alpha=0.7)
plt.setp(ax2.xaxis.get_majorticklabels(), rotation=45, ha='right')

# 3. Bar chart comparing average overexposure
ax3 = axes[1, 0]
watersheds = list(class_names)
avg_overexp = [overexposure_stats[name]['avg'] for name in watersheds]
std_overexp = [overexposure_stats[name]['std'] for name in watersheds]

bars = ax3.bar(range(len(watersheds)), avg_overexp, yerr=std_overexp,
               capsize=5, color=plt.cm.Set3(range(len(class_names))),
               edgecolor='black', alpha=0.7)
ax3.set_xlabel('Watershed', fontweight='bold')
ax3.set_ylabel('Average Overexposed Pixels (%) ± Std', fontweight='bold')
ax3.set_title('Average Overexposure by Watershed',
              fontsize=12, fontweight='bold')
ax3.set_xticks(range(len(watersheds)))
ax3.set_xticklabels(watersheds, rotation=45, ha='right')
ax3.grid(True, alpha=0.3, axis='y')
ax3.axhline(y=mean_of_overexposures, color='red', linestyle='--',
            label=f'Overall Mean: {mean_of_overexposures:.2f}%')
ax3.legend()

# 4. Proportion of problematic images per watershed
ax4 = axes[1, 1]
x_pos = np.arange(len(class_names))
width = 0.35

moderate_pct = [overexposure_stats[name]['n_moderate']/overexposure_stats[name]['total_images']*100
                for name in class_names]
significant_pct = [overexposure_stats[name]['n_significant']/overexposure_stats[name]['total_images']*100
                   for name in class_names]

bars1 = ax4.bar(x_pos - width/2, moderate_pct, width, label='>5% overexposed',
                color='orange', alpha=0.7, edgecolor='black')
bars2 = ax4.bar(x_pos + width/2, significant_pct, width, label='>10% overexposed',
                color='red', alpha=0.7, edgecolor='black')

ax4.set_xlabel('Watershed', fontweight='bold')
ax4.set_ylabel('Percentage of Images', fontweight='bold')
ax4.set_title('Proportion of Overexposed Images by Watershed',
              fontsize=12, fontweight='bold')
ax4.set_xticks(x_pos)
ax4.set_xticklabels(class_names, rotation=45, ha='right')
ax4.legend()
ax4.grid(True, alpha=0.3, axis='y')

plt.tight_layout()
plt.savefig(os.path.join(figures_path, 'overexposure_analysis.png'),
            dpi=300, bbox_inches='tight')
plt.close()

print(
    f"\n✓ Overexposure analysis plots saved to: {os.path.join(figures_path, 'overexposure_analysis.png')}")
print("="*80 + "\n")

# =============================================================================
# FILTER OVEREXPOSED IMAGES
# =============================================================================

print("\n" + "="*80)
print("FILTERING HEAVILY OVEREXPOSED IMAGES")
print("="*80)

# Define threshold for filtering (images with >10% overexposed pixels)
overexposure_filter_threshold = 0.10

# Calculate overexposure for each image
overexposed_pixels_per_image = np.mean(x_data > 0.95, axis=(1, 2, 3))
overexposed_mask = overexposed_pixels_per_image > overexposure_filter_threshold

print(
    f"\nFiltering criteria: Remove images with >{overexposure_filter_threshold*100:.0f}% overexposed pixels")
print(f"Images before filtering: {len(x_data)}")
print(f"Images flagged for removal: {np.sum(overexposed_mask)}")

# Show breakdown by watershed before filtering
print("\nBreakdown by watershed BEFORE filtering:")
for watershed_idx, watershed_name in enumerate(class_names):
    watershed_mask = y_data == watershed_idx
    n_original = np.sum(watershed_mask)
    n_overexposed = np.sum(watershed_mask & overexposed_mask)
    print(f"  {watershed_name}: {n_original} images ({n_overexposed} overexposed, {n_overexposed/n_original*100:.1f}%)")

# Apply filter
x_data_filtered = x_data[~overexposed_mask]
y_data_filtered = y_data[~overexposed_mask]

print(f"\nImages after filtering: {len(x_data_filtered)}")
print(
    f"Total images removed: {len(x_data) - len(x_data_filtered)} ({(len(x_data) - len(x_data_filtered))/len(x_data)*100:.1f}%)")

# Check class balance after filtering
print("\nBreakdown by watershed AFTER filtering:")
for watershed_idx, watershed_name in enumerate(class_names):
    n_remaining = np.sum(y_data_filtered == watershed_idx)
    n_original = np.sum(y_data == watershed_idx)
    n_removed = n_original - n_remaining
    print(f"  {watershed_name}: {n_remaining}/{n_original} remaining ({n_remaining/n_original*100:.1f}%, removed {n_removed})")

# Check if any class has too few samples
min_samples_per_class = np.min(
    [np.sum(y_data_filtered == i) for i in range(len(class_names))])
if min_samples_per_class < 20:
    print(f"\n⚠️  WARNING: Some classes have fewer than 20 samples after filtering!")
    print(f"    Consider using a higher threshold or applying preprocessing instead.")
else:
    print(
        f"\n✓ All classes have sufficient samples (min: {min_samples_per_class})")

# Visualize filtering impact
fig, axes = plt.subplots(1, 2, figsize=(15, 6))

# Before filtering
ax1 = axes[0]
for watershed_idx, watershed_name in enumerate(class_names):
    n_images = np.sum(y_data == watershed_idx)
    ax1.bar(watershed_name, n_images, color=plt.cm.Set3(watershed_idx),
            edgecolor='black', alpha=0.7)
ax1.set_ylabel('Number of Images', fontweight='bold')
ax1.set_xlabel('Watershed', fontweight='bold')
ax1.set_title('Dataset Size BEFORE Filtering', fontsize=12, fontweight='bold')
ax1.grid(True, alpha=0.3, axis='y')

# After filtering
ax2 = axes[1]
for watershed_idx, watershed_name in enumerate(class_names):
    n_images = np.sum(y_data_filtered == watershed_idx)
    ax2.bar(watershed_name, n_images, color=plt.cm.Set3(watershed_idx),
            edgecolor='black', alpha=0.7)
ax2.set_ylabel('Number of Images', fontweight='bold')
ax2.set_xlabel('Watershed', fontweight='bold')
ax2.set_title('Dataset Size AFTER Filtering', fontsize=12, fontweight='bold')
ax2.grid(True, alpha=0.3, axis='y')

plt.tight_layout()
plt.savefig(os.path.join(figures_path, 'filtering_impact.png'),
            dpi=300, bbox_inches='tight')
plt.close()

print(
    f"\n✓ Filtering impact visualization saved to: {os.path.join(figures_path, 'filtering_impact.png')}")

# Use filtered data for training
x_data = x_data_filtered
y_data = y_data_filtered

print("="*80 + "\n")

# =============================================================================
# PREPARE DATA
# =============================================================================

x_train, x_test, y_train, y_test = train_test_split(
    x_data, y_data, test_size=0.2, random_state=42, stratify=y_data
)

input_shape = (128, 128, 1)
num_classes = len(class_names)
y_train_categorical = keras.utils.to_categorical(y_train, num_classes)
y_test_categorical = keras.utils.to_categorical(y_test, num_classes)

print(f"Training set: {x_train.shape[0]} images")
print(f"Test set: {x_test.shape[0]} images")

# =============================================================================
# BUILD MODEL
# =============================================================================

model = keras.Sequential([
    keras.Input(shape=input_shape),

    layers.Conv2D(filters=32, kernel_size=(3, 3), activation="relu"),
    layers.MaxPooling2D(pool_size=(2, 2)),

    layers.Conv2D(filters=64, kernel_size=(3, 3), activation="relu"),
    layers.MaxPooling2D(pool_size=(2, 2)),

    layers.Conv2D(filters=128, kernel_size=(3, 3), activation="relu"),
    layers.MaxPooling2D(pool_size=(2, 2)),

    layers.Flatten(),
    layers.Dropout(0.5),
    layers.Dense(128, activation="relu"),
    layers.Dropout(0.3),
    layers.Dense(num_classes, activation="softmax"),
])

model.summary()

# =============================================================================
# TRAIN MODEL (SIMPLIFIED APPROACH)
# =============================================================================

batch_size = 32
epochs = 30

early_stopping = keras.callbacks.EarlyStopping(
    monitor='val_loss',
    patience=3,
    min_delta=0.01,
    restore_best_weights=True,
    verbose=1
)

model.compile(
    loss="categorical_crossentropy",
    optimizer=keras.optimizers.Adam(learning_rate=0.0005),
    metrics=["accuracy"]
)

print("Training model with early stopping...")

history = model.fit(
    x_train, y_train_categorical,
    batch_size=batch_size,
    epochs=epochs,
    validation_split=0.2,
    callbacks=[early_stopping],
    verbose=1
)

final_train_acc = history.history['accuracy'][-1]
final_val_acc = history.history['val_accuracy'][-1]
print(f"Training Accuracy: {final_train_acc:.1%}")
print(f"Validation Accuracy: {final_val_acc:.1%}")

# =============================================================================
# EVALUATE MODEL
# =============================================================================

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(15, 5))

epochs_range = range(1, len(history.history["accuracy"]) + 1)
ax1.plot(epochs_range, history.history["accuracy"],
         'b-', label='Training Accuracy', linewidth=2)
ax1.plot(epochs_range, history.history["val_accuracy"],
         'r-', label='Validation Accuracy', linewidth=2)
ax1.set_title('Training and Validation Accuracy',
              fontsize=14, fontweight='bold')
ax1.set_xlabel('Epochs')
ax1.set_ylabel('Accuracy')
ax1.legend()
ax1.grid(True, alpha=0.3)
ax1.set_ylim([0, 1])

ax2.plot(epochs_range, history.history["loss"],
         'b-', label='Training Loss', linewidth=2)
ax2.plot(epochs_range, history.history["val_loss"],
         'r-', label='Validation Loss', linewidth=2)
ax2.set_title('Training and Validation Loss', fontsize=14, fontweight='bold')
ax2.set_xlabel('Epochs')
ax2.set_ylabel('Loss')
ax2.legend()
ax2.grid(True, alpha=0.3)

plt.tight_layout()
plt.savefig(os.path.join(figures_path, 'training_history.png'),
            dpi=300, bbox_inches='tight')
plt.close()

test_loss, test_accuracy = model.evaluate(
    x_test, y_test_categorical, verbose=0)
print(f"Test Accuracy: {test_accuracy:.1%}")

test_predictions = model.predict(x_test, verbose=0)
predicted_classes = np.argmax(test_predictions, axis=1)
true_classes = np.argmax(y_test_categorical, axis=1)

cm = confusion_matrix(true_classes, predicted_classes)
cm_percent = cm.astype('float') / cm.sum(axis=1)[:, np.newaxis] * 100

plt.figure(figsize=(8, 6))
annot = np.array([[f'{count}\n({pct:.1f}%)' for count, pct in zip(row_counts, row_pcts)]
                  for row_counts, row_pcts in zip(cm, cm_percent)])
sns.heatmap(cm, annot=annot, fmt='', cmap='Blues',
            xticklabels=class_names, yticklabels=class_names,
            cbar_kws={'label': 'Number of Otoliths'})
plt.title('Confusion Matrix: Otolith Classification Results',
          fontsize=14, fontweight='bold')
plt.xlabel('Predicted Watershed')
plt.ylabel('True Watershed')
plt.savefig(os.path.join(figures_path, 'confusion_matrix.png'),
            dpi=300, bbox_inches='tight')
plt.close()

print("Classification Report:")
report = classification_report(
    true_classes, predicted_classes, target_names=class_names, digits=3)
print(report)

print("Analysis by watershed:")
for i, watershed in enumerate(class_names):
    watershed_indices = np.where(true_classes == i)[0]
    watershed_predictions = predicted_classes[watershed_indices]
    correct_predictions = np.sum(watershed_predictions == i)
    total_predictions = len(watershed_indices)
    watershed_accuracy = correct_predictions / \
        total_predictions if total_predictions > 0 else 0

    print(f"{watershed}: {correct_predictions}/{total_predictions} ({watershed_accuracy:.1%})")

prediction_confidence = np.max(test_predictions, axis=1)
print(f"Average prediction confidence: {np.mean(prediction_confidence):.1%}")

plt.figure(figsize=(10, 6))
plt.hist(prediction_confidence, bins=20, alpha=0.7,
         color='skyblue', edgecolor='black')
plt.axvline(np.mean(prediction_confidence), color='red', linestyle='--',
            label=f'Average: {np.mean(prediction_confidence):.1%}')
plt.title('Distribution of Prediction Confidence',
          fontsize=14, fontweight='bold')
plt.xlabel('Confidence (Maximum Probability)')
plt.ylabel('Number of Predictions')
plt.legend()
plt.grid(True, alpha=0.3)
plt.savefig(os.path.join(figures_path, 'prediction_confidence.png'),
            dpi=300, bbox_inches='tight')
plt.close()

model_save_path = "otolith_classifier_model.h5"
model.save(model_save_path)
print(f"Model saved as: {model_save_path}")
