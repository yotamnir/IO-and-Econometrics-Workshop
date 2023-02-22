# Draft page for the assignment

#NLS1 <- NLS %>%
#  filter(!between(ed76, 13, 15))

K0 <- min(NLS$lwage76)  # absolute lower bound
K1 <- max(NLS$lwage76)  # absolute upper bound

z <- (NLS$ed76 > 12)    # treatment: 1 if above 12 years of schooling, 0 otherwise
y <- NLS$lwage76        # outcome: unchanged
v <- NLS$nearc4         # IV: unchanged for now

Ey_z1 <- mean(y[z==1])  # sample analogue for E[y|z=1]
Ey_z0 <- mean(y[z==0])  # sample analogue for E[y|z=0]
Pz1 <- mean(z)          # sample analogue for P(z=1)
Pz0 <- 1 - mean(z)      # sample analogue for P(z=0)


########################################################
#             Natural bounds on ATE
########################################################
# Lower bound on E[y(1)]:
b1 <- Ey_z1 * Pz1 + K0 * Pz0
# Upper bound on E[y(1)]:
B1 <- Ey_z1 * Pz1 + K1 * Pz0
# Lower bound on E[y(0)]:
b0 <- Ey_z0 * Pz0 + K0 * Pz1
# Upper bound on E[y(0)]:
B0 <- Ey_z0 * Pz0 + K1 * Pz1

# ATE lower bound:
b1 - B0
# ATE upper bound:
B1 - b0

#############################################################
#           Adding (regular) IV assumption
#############################################################
Ey_z1v1 <- mean(y[z==1 & v==1]) # sample analogue for E[y|z=1, v=1]
Ey_z1v0 <- mean(y[z==1 & v==0]) # sample analogue for E[y|z=1, v=0]
Ey_z0v1 <- mean(y[z==0 & v==1]) # sample analogue for E[y|z=0, v=1]
Ey_z0v0 <- mean(y[z==0 & v==0]) # sample analogue for E[y|z=0, v=0]
Pz1_v1 <- mean(z[v==1])         # sample analogue for P(z=1|v=1)
Pz1_v0 <- mean(z[v==0])         # sample analogue for P(z=1|v=0)
Pz0_v1 <- mean(z[v==1]==0)      # sample analogue for P(z=0|v=1)
Pz0_v0 <- mean(z[v==0]==0)      # sample analogue for P(z=0|v=0)

# Reminder: we can choose the maximal lower bounds / minimal upper bounds
# since we are assuming invariance conditional on v
maxb1 = max(Ey_z1v1 * Pz1_v1 + K0 * Pz0_v1,   # for v = 1
            Ey_z1v0 * Pz1_v0 + K0 * Pz0_v0)   # for v = 0
maxb0 = max(Ey_z0v1 * Pz0_v1 + K0 * Pz1_v1,   # for v = 1
            Ey_z0v0 * Pz0_v0 + K0 * Pz1_v0)   # for v = 0
minB1 = min(Ey_z1v1 * Pz1_v1 + K1 * Pz0_v1,   # for v = 1
            Ey_z1v0 * Pz1_v0 + K1 * Pz0_v0)   # for v = 0
minB0 = min(Ey_z0v1 * Pz0_v1 + K1 * Pz1_v1,   # for v = 1
            Ey_z0v0 * Pz0_v0 + K1 * Pz1_v0)   # for v = 0

# ATE lower bound:
maxb1 - minB0
# ATE upper bound:
minB1 - maxb0

#############################################################
#           Relaxing IV assumption to MIV
#############################################################

# Bounds given v = 0 (lower bound can only be with v = 0, upper bound can be with either 0 or 1):
b1_v0 = Ey_z1v0 * Pz1_v0 + K0 * Pz0_v0
b0_v0 = Ey_z0v0 * Pz0_v0 + K0 * Pz1_v0
B1_v0 = min(Ey_z1v1 * Pz1_v1 + K1 * Pz0_v1,   # for v = 1
            Ey_z1v0 * Pz1_v0 + K1 * Pz0_v0)   # for v = 0
B0_v0 = min(Ey_z0v1 * Pz0_v1 + K1 * Pz1_v1,   # for v = 1
            Ey_z0v0 * Pz0_v0 + K1 * Pz1_v0)   # for v = 0
# Lower bound:
b1_v0 - B0_v0
# Upper bound:
B1_v0 - b0_v0

# Bounds given v = 1 (lower bound can be with either 0 or 1, upper bound can only be with v = 1):
b1_v1 = max(Ey_z1v1 * Pz1_v1 + K0 * Pz0_v1,   # for v = 1
            Ey_z1v0 * Pz1_v0 + K0 * Pz0_v0)   # for v = 0
b0_v1 = max(Ey_z0v1 * Pz0_v1 + K0 * Pz1_v1,   # for v = 1
            Ey_z0v0 * Pz0_v0 + K0 * Pz1_v0)   # for v = 0
B1_v1 = Ey_z1v1 * Pz1_v1 + K1 * Pz0_v1
B0_v1 = Ey_z0v1 * Pz0_v1 + K1 * Pz1_v1
# Lower bound:
b1_v1 - B0_v1
# Upper bound:
B1_v1 - b0_v1


### MIV bounds on ATE:
Pv0 <- mean(v==0)
Pv1 <- mean(v)
# Lower bound:
(b1_v0 - B0_v0) * Pv0 + (b1_v1 - B0_v1) * Pv1
# Upper bound:
(B1_v0 - b0_v0) * Pv0 + (B1_v1 - b0_v1) * Pv1


###############################################################
# Adding MTS assumption: E[y(t)|v=u, z=1] >= E[y(t)|v=u, z=0]
###############################################################
# Bounds given v = 0:
b1_v0MTS = Ey_z1v0 * Pz1_v0 + Ey_z0v0 * Pz0_v0
b0_v0MTS = Ey_z0v0
B1_v0MTS = min(Ey_z1v1,   # for v = 1
               Ey_z1v0)   # for v = 0
B0_v0MTS = min(Ey_z0v1 * Pz0_v1 + Ey_z1v1 * Pz1_v1,   # for v = 1
               Ey_z0v0 * Pz0_v0 + Ey_z1v0 * Pz1_v0)   # for v = 0
# Lower bound:
b1_v0MTS - B0_v0MTS
# Upper bound:
B1_v0MTS - b0_v0MTS

# Bounds given v = 1:
b1_v1MTS = max(Ey_z1v1 * Pz1_v1 + Ey_z0v1 * Pz0_v1,   # for v = 1
               Ey_z1v0 * Pz1_v0 + Ey_z0v0 * Pz0_v0)   # for v = 0
b0_v1MTS = max(Ey_z0v1,   # for v = 1
               Ey_z0v0)   # for v = 0
B1_v1MTS = Ey_z1v1
B0_v1MTS = Ey_z0v1 * Pz0_v1 + Ey_z1v1 * Pz1_v1
# Lower bound:
b1_v1MTS - B0_v1MTS
# Upper bound:
B1_v1MTS - b0_v1MTS


### MIV-MTS bounds on ATE:

# Lower bound:
(b1_v0MTS - B0_v0MTS) * Pv0 + (b1_v1MTS - B0_v1MTS) * Pv1
# Upper bound:
(B1_v0MTS - b0_v0MTS) * Pv0 + (B1_v1MTS - b0_v1MTS) * Pv1







# Additional checks not relevant to the above workflow
ggplot(data = NLS %>% group_by(ed76) %>% summarise(mean = mean(V39))) +
  geom_point(mapping = aes(ed76, mean))


