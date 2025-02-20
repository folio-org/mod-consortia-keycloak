package org.folio.consortia.domain.dto;

import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.With;

@Data
@With
@AllArgsConstructor
@NoArgsConstructor
public class RealmExecutions {
  private String id;
  private String requirement;
  private String displayName;
  private List<String> requirementChoices;
  private boolean configurable;
  private String providerId;
  private int level;
  private int index;
  private int priority;
}
