# =============================================================
# Diffusion d'un polluant (lac Nyos) - solution exacte 2D
# Version qui AFFICHE les graphiques dans l'onglet Plots de RStudio
# puis tente de les enregistrer et dit OU.
# =============================================================

C_ana <- function(x, y, t, M = 1, D = 0.1, sigma0 = 0.5) {
  s2 <- sigma0^2 + 2 * D * t
  M / (2 * pi * s2) * exp(-(x^2 + y^2) / (2 * s2))
}

M <- 1; D <- 0.1; sigma0 <- 0.5
temps <- c(0, 5, 20)
L <- 6
xs <- seq(-L, L, length.out = 201)
ys <- seq(-L, L, length.out = 201)
palette_nyos <- colorRampPalette(c("#0F2A44", "#1A6F94", "#7FD4E8"))(256)
zmax <- C_ana(0, 0, 0, M, D, sigma0)

# ---- Graphique 1 : les 3 cartes (slide 11) ------------------
dessiner_cartes <- function() {
  par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))
  for (t in temps) {
    Z <- outer(xs, ys, function(x, y) C_ana(x, y, t, M, D, sigma0))
    image(xs, ys, Z, col = palette_nyos, zlim = c(0, zmax), asp = 1,
          xlab = "x", ylab = "y",
          main = sprintf("t = %d  (pic = %.3f)", t, max(Z)))
  }
}

# ---- Graphique 2 : effet de D (slide 12) --------------------
dessiner_sensibilite <- function() {
  par(mfrow = c(1, 1), mar = c(4.5, 4.5, 3, 1))
  D_vals <- c(0.05, 0.1, 0.2)
  cols <- c("#7FD4E8", "#1A6F94", "#E8A33D")
  x_fin <- seq(-5, 5, length.out = 400)
  plot(NA, xlim = c(-5, 5), ylim = c(0, 0.14),
       xlab = "x  (y = 0)", ylab = "Concentration C",
       main = "Effet du coefficient D  (t = 10)")
  for (i in seq_along(D_vals)) {
    lines(x_fin, C_ana(x_fin, 0, 10, M, D_vals[i], sigma0),
          col = cols[i], lwd = 3)
  }
  legend("topright", legend = paste("D =", D_vals), col = cols, lwd = 3, bty = "n")
  grid()
}

# ---- Affichage a l'ecran (onglet Plots) ---------------------
dessiner_cartes()
dessiner_sensibilite()   # utilise les fleches de l'onglet Plots pour revenir au 1er

# ---- Enregistrement (avec message clair) --------------------
dossier <- file.path(getwd(), "figures")
dir.create(dossier, showWarnings = FALSE)

sauver <- function(nom, largeur, hauteur, fonction) {
  chemin <- file.path(dossier, nom)
  ok <- tryCatch({
    png(chemin, width = largeur, height = hauteur)
    fonction()
    dev.off()
    TRUE
  }, error = function(e) {
    message("Echec pour ", nom, " : ", conditionMessage(e))
    while (dev.cur() > 1) dev.off()
    FALSE
  })
  if (ok && file.exists(chemin)) message("OK : ", chemin)
}

sauver("cartes_comparaison.png", 1500, 520, dessiner_cartes)
sauver("sensibilite_D.png", 900, 600, dessiner_sensibilite)

message("Dossier de travail : ", getwd())