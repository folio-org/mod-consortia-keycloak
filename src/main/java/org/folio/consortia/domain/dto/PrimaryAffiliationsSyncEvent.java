package org.folio.consortia.domain.dto;

import java.util.UUID;

public record PrimaryAffiliationsSyncEvent(UUID consortiumId, String tenantId, String centralTenantId) {
}
