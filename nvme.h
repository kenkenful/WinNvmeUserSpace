#pragma once



typedef INT8             s8;         ///< 8-bit signed
typedef INT16             s16;        ///< 16-bit signed
typedef INT32             s32;        ///< 32-bit signed
typedef INT64             s64;        ///< 64-bit signed
typedef UINT8             u8;         ///< 8-bit unsigned
typedef UINT16            u16;        ///< 16-bit unsigned
typedef UINT32            u32;        ///< 32-bit unsigned
typedef UINT64            u64;        ///< 64-bit unsigned


#define NVME_NVM_IOSQES		6
#define NVME_NVM_IOCQES		4


enum {
	NVME_CC_ENABLE = 1 << 0,
	NVME_CC_CSS_NVM = 0 << 4,
	NVME_CC_EN_SHIFT = 0,
	NVME_CC_CSS_SHIFT = 4,
	NVME_CC_MPS_SHIFT = 7,
	NVME_CC_AMS_SHIFT = 11,
	NVME_CC_SHN_SHIFT = 14,
	NVME_CC_IOSQES_SHIFT = 16,
	NVME_CC_IOCQES_SHIFT = 20,
	NVME_CC_AMS_RR = 0 << NVME_CC_AMS_SHIFT,
	NVME_CC_AMS_WRRU = 1 << NVME_CC_AMS_SHIFT,
	NVME_CC_AMS_VS = 7 << NVME_CC_AMS_SHIFT,
	NVME_CC_SHN_NONE = 0 << NVME_CC_SHN_SHIFT,
	NVME_CC_SHN_NORMAL = 1 << NVME_CC_SHN_SHIFT,
	NVME_CC_SHN_ABRUPT = 2 << NVME_CC_SHN_SHIFT,
	NVME_CC_SHN_MASK = 3 << NVME_CC_SHN_SHIFT,
	NVME_CC_IOSQES = NVME_NVM_IOSQES << NVME_CC_IOSQES_SHIFT,
	NVME_CC_IOCQES = NVME_NVM_IOCQES << NVME_CC_IOCQES_SHIFT,
	NVME_CSTS_RDY = 1 << 0,
	NVME_CSTS_CFS = 1 << 1,
	NVME_CSTS_NSSRO = 1 << 4,
	NVME_CSTS_PP = 1 << 5,
	NVME_CSTS_SHST_NORMAL = 0 << 2,
	NVME_CSTS_SHST_OCCUR = 1 << 2,
	NVME_CSTS_SHST_CMPLT = 2 << 2,
	NVME_CSTS_SHST_MASK = 3 << 2,
};



enum nvme_opcode {
	nvme_cmd_flush = 0x00,
	nvme_cmd_write = 0x01,
	nvme_cmd_read = 0x02,
	nvme_cmd_write_uncor = 0x04,
	nvme_cmd_compare = 0x05,
	nvme_cmd_write_zeroes = 0x08,
	nvme_cmd_dsm = 0x09,
	nvme_cmd_verify = 0x0c,
	nvme_cmd_resv_register = 0x0d,
	nvme_cmd_resv_report = 0x0e,
	nvme_cmd_resv_acquire = 0x11,
	nvme_cmd_resv_release = 0x15,
	nvme_cmd_zone_mgmt_send = 0x79,
	nvme_cmd_zone_mgmt_recv = 0x7a,
	nvme_cmd_zone_append = 0x7d,
	nvme_cmd_vendor_start = 0x80,
};

enum nvme_admin_opcode {
	nvme_admin_delete_sq = 0x00,
	nvme_admin_create_sq = 0x01,
	nvme_admin_get_log_page = 0x02,
	nvme_admin_delete_cq = 0x04,
	nvme_admin_create_cq = 0x05,
	nvme_admin_identify = 0x06,
	nvme_admin_abort_cmd = 0x08,
	nvme_admin_set_features = 0x09,
	nvme_admin_get_features = 0x0a,
	nvme_admin_async_event = 0x0c,
	nvme_admin_ns_mgmt = 0x0d,
	nvme_admin_activate_fw = 0x10,
	nvme_admin_download_fw = 0x11,
	nvme_admin_dev_self_test = 0x14,
	nvme_admin_ns_attach = 0x15,
	nvme_admin_keep_alive = 0x18,
	nvme_admin_directive_send = 0x19,
	nvme_admin_directive_recv = 0x1a,
	nvme_admin_virtual_mgmt = 0x1c,
	nvme_admin_nvme_mi_send = 0x1d,
	nvme_admin_nvme_mi_recv = 0x1e,
	nvme_admin_dbbuf = 0x7C,
	nvme_admin_format_nvm = 0x80,
	nvme_admin_security_send = 0x81,
	nvme_admin_security_recv = 0x82,
	nvme_admin_sanitize_nvm = 0x84,
	nvme_admin_get_lba_status = 0x86,
	nvme_admin_vendor_start = 0xC0,
};


typedef struct nvme_sgl_desc {
	u64	addr;
	u32	length;
	u8	rsvd[3];
	u8	type;
}nvme_sgl_desc_t;

typedef struct nvme_keyed_sgl_desc {
	u64	addr;
	u8	length[3];
	u8	key[4];
	u8	type;
}nvme_keyed_sgl_desc_t;

union nvme_data_ptr {
	//struct s{
		u64	prp1;
		u64	prp2;
	//};
	//struct nvme_sgl_desc	sgl;
	//struct nvme_keyed_sgl_desc ksgl;
}u;


typedef struct nvme_common_command {
	u8			opcode;
	u8			flags;
	u16			command_id;
	u32			nsid;
	u32			cdw2[2];
	u64			metadata;
	u64			prp1;
	u64			prp2;
	u32			cdw10[6];
}nvme_common_command_t;

typedef struct nvme_rw_command {
	u8			opcode;
	u8			flags;
	u16			command_id;
	u32			nsid;
	u64			rsvd2;
	u64			metadata;
	u64			prp1;
	u64			prp2;
	u64			slba;
	u16			length;
	u16			control;
	u32			dsmgmt;
	u32			reftag;
	u16			apptag;
	u16			appmask;
}nvme_rw_command_t;


typedef struct nvme_identify {
	u8			opcode;
	u8			flags;
	u16			command_id;
	u32			nsid;
	u64			rsvd2[2];
	u64			prp1;
	u64			prp2;
	u32			cns;
	u32			rsvd11[5];
}nvme_identify_t;

typedef struct nvme_features {
	u8			opcode;
	u8			flags;
	u16			command_id;
	u32			nsid;
	u64			rsvd2[2];
	u64			prp1;
	u64			prp2;
	u32			fid;
	u32			dword11;
	u32			rsvd12[4];
}nvme_features_t;

typedef struct nvme_create_cq {
	u8			opcode;
	u8			flags;
	u16			command_id;
	u32			rsvd1[5];
	u64			prp1;
	u64			rsvd8;
	u16			cqid;
	u16			qsize;
	u16			cq_flags;
	u16			irq_vector;
	u32			rsvd12[4];
}nvme_create_cq_t;

typedef struct nvme_create_sq {
	u8			opcode;
	u8			flags;
	u16			command_id;
	u32			rsvd1[5];
	u64			prp1;
	u64			rsvd8;
	u16			sqid;
	u16			qsize;
	u16			sq_flags;
	u16			cqid;
	u32			rsvd12[4];
}nvme_create_sq_t;

typedef struct nvme_delete_queue {
	u8			opcode;
	u8			flags;
	u16			command_id;
	u32			rsvd1[9];
	u16			qid;
	u16			rsvd10;
	u32			rsvd11[5];
}nvme_delete_queue_t;

typedef struct nvme_abort_cmd {
	u8			opcode;
	u8			flags;
	u16			command_id;
	u32			rsvd1[9];
	u16			sqid;
	u16			cid;
	u32			rsvd11[5];
}nvme_abort_cmd_t;

typedef struct nvme_download_firmware {
	u8			opcode;
	u8			flags;
	u16			command_id;
	u32			rsvd1[5];
	u64			prp1;
	u64			prp2;
	u32			numd;
	u32			offset;
	u32			rsvd12[4];
}nvme_download_firmware_t;

typedef struct nvme_format_cmd {
	u8			opcode;
	u8			flags;
	u16			command_id;
	u32			nsid;
	u64			rsvd2[4];
	u32			cdw10;
	u32			rsvd11[5];
}nvme_format_cmd_t;

typedef struct nvme_dsm_cmd {
	u8			opcode;
	u8			flags;
	u16			command_id;
	u32			nsid;
	u64			rsvd2[2];
	u64			prp1;
	u64			prp2;
	u32			nr;
	u32			attributes;
	u32			rsvd12[4];
}nvme_dsm_cmd_t;



typedef struct nvme_get_log_page_command {
	u8			opcode;
	u8			flags;
	u16			command_id;
	u32			nsid;
	u64			rsvd2[2];
	union nvme_data_ptr	dptr;
	u8			lid;
	u8			lsp; /* upper 4 bits reserved */
	u16			numdl;
	u16			numdu;
	u16			rsvd11;
	union {
		struct s{
			u32 lpol;
			u32 lpou;
		};
		u64 lpo;
	}u;
	u8			rsvd14[3];
	u8			csi;
	u32			rsvd15;
}nvme_get_log_page_command_t;



typedef struct nvme_write_zeroes_cmd {
	u8			opcode;
	u8			flags;
	u16			command_id;
	u32			nsid;
	u64			rsvd2;
	u64			metadata;
	union nvme_data_ptr	dptr;
	u64			slba;
	u16			length;
	u16			control;
	u32			dsmgmt;
	u32			reftag;
	u16			apptag;
	u16			appmask;
}nvme_write_zeroes_cmd_t;


typedef struct nvme_zone_mgmt_send_cmd {
	u8			opcode;
	u8			flags;
	u16			command_id;
	u32			nsid;
	u32			cdw2[2];
	u64			metadata;
	union nvme_data_ptr	dptr;
	u64			slba;
	u32			cdw12;
	u8			zsa;
	u8			select_all;
	u8			rsvd13[2];
	u32			cdw14[2];
}nvme_zone_mgmt_send_cmd_t;


typedef struct nvme_zone_mgmt_recv_cmd {
	u8			opcode;
	u8			flags;
	u16			command_id;
	u32			nsid;
	u64			rsvd2[2];
	union nvme_data_ptr	dptr;
	u64			slba;
	u32			numd;
	u8			zra;
	u8			zrasf;
	u8			pr;
	u8			rsvd13;
	u32			cdw14[2];
}nvme_zone_mgmt_recv_cmd_t;

typedef struct nvmf_common_command {
	u8	opcode;
	u8	resv1;
	u16	command_id;
	u8	fctype;
	u8	resv2[35];
	u8	ts[24];
}nvmf_common_command_t;


typedef struct nvmf_connect_command {
	u8		opcode;
	u8		resv1;
	u16		command_id;
	u8		fctype;
	u8		resv2[19];
	union nvme_data_ptr dptr;
	u16		recfmt;
	u16		qid;
	u16		sqsize;
	u8		cattr;
	u8		resv3;
	u32		kato;
	u8		resv4[12];
}nvmf_connect_command_t;

typedef struct nvmf_property_set_command {
	u8		opcode;
	u8		resv1;
	u16		command_id;
	u8		fctype;
	u8		resv2[35];
	u8		attrib;
	u8		resv3[3];
	u32		offset;
	u64		value;
	u8		resv4[8];
}nvmf_property_set_command_t;


typedef struct nvmf_property_get_command {
	u8		opcode;
	u8		resv1;
	u16		command_id;
	u8		fctype;
	u8		resv2[35];
	u8		attrib;
	u8		resv3[3];
	u32		offset;
	u8		resv4[16];
}nvmf_property_get_command_t;

typedef struct nvme_dbbuf {
	u8			opcode;
	u8			flags;
	u16			command_id;
	u32			rsvd1[5];
	u64			prp1;
	u64			prp2;
	u32			rsvd12[6];
}nvme_dbbuf;

typedef struct streams_directive_params {
	u16	msl;
	u16	nssa;
	u16	nsso;
	u8	rsvd[10];
	u32	sws;
	u16	sgs;
	u16	nsa;
	u16	nso;
	u8	rsvd2[6];
}streams_directive_params_t;

typedef struct nvme_directive_cmd {
	u8			opcode;
	u8			flags;
	u16			command_id;
	u32			nsid;
	u64			rsvd2[2];
	union nvme_data_ptr	dptr;
	u32			numd;
	u8			doper;
	u8			dtype;
	u16			dspec;
	u8			endir;
	u8			tdtype;
	u16			rsvd15;

	u32			rsvd16[3];
}nvme_directive_cmd_t;

typedef struct nvme_command {
	union {
		struct nvme_common_command common;
		struct nvme_rw_command rw;
		struct nvme_identify identify;
		struct nvme_features features;
		struct nvme_create_cq create_cq;
		struct nvme_create_sq create_sq;
		struct nvme_delete_queue delete_queue;
		struct nvme_download_firmware dlfw;
		struct nvme_format_cmd format;
		struct nvme_dsm_cmd dsm;
		struct nvme_write_zeroes_cmd write_zeroes;
		struct nvme_zone_mgmt_send_cmd zms;
		struct nvme_zone_mgmt_recv_cmd zmr;
		struct nvme_abort_cmd abort;
		struct nvme_get_log_page_command get_log_page;
		struct nvmf_common_command fabrics;
		struct nvmf_connect_command connect;
		struct nvmf_property_set_command prop_set;
		struct nvmf_property_get_command prop_get;
		struct nvme_dbbuf dbbuf;
		struct nvme_directive_cmd directive;
	}u;
}nvme_command_t;



/* nvme controller register */

typedef union _nvme_version {
	u32                 val;
	struct s{
		u8              rsvd;
		u8              mnr;
		u16             mjr;
	};
} nvme_version_t;

 struct attr_ctx{
	u16             asqs;
	u16             acqs;
};


typedef union _nvme_adminq_attr {
	u32                 val;
	struct attr_ctx   a;
} nvme_adminq_attr_t;


struct cap_ctx {
	u16             mqes;
	u8              cqr : 1;
	u8              ams : 2;
	u8              rsvd : 5;
	u8              to;

	u32             dstrd : 4;
	u32             nssrs : 1;
	u32             css : 8;
	u32             rsvd2 : 3;
	u32             mpsmin : 4;
	u32             mpsmax : 4;
	u32             rsvd3 : 8;
};


typedef union _nvme_controller_cap {
	u64                 val;
	struct cap_ctx a;
	
} nvme_controller_cap_t;


struct cc_ctx {
	u32             en : 1;
	u32             rsvd : 3;
	u32             css : 3;
	u32             mps : 4;
	u32             ams : 3;
	u32             shn : 2;
	u32             iosqes : 4;
	u32             iocqes : 4;
	u32             rsvd2 : 8;
};


typedef union _nvme_controller_config {
	u32                 val;
	struct cc_ctx   a;
} nvme_controller_config_t;

typedef union _nvme_controller_status {
	//u32                 val;
	//struct s{
		u32             rdy : 1;
		u32             cfs : 1;
		u32             shst : 2;
		u32             rsvd : 28;
	//};
} nvme_controller_status_t;

typedef struct _nvme_controller_reg {
	nvme_controller_cap_t   cap;
	nvme_version_t          vs;
	u32                     intms;
	u32                     intmc;
	nvme_controller_config_t cc;
	u32                     rsvd;
	nvme_controller_status_t csts;
	u32                     nssr;
	nvme_adminq_attr_t      aqa;
	u64                     asq;
	u64                     acq;
	u32                     rcss[1010];
	u32                     sq0tdbl[1024];
} nvme_controller_reg_t;



typedef union _nvme_sq_entry {

	struct nvme_common_command common;
	struct nvme_rw_command rw;
	struct nvme_identify identify;
	struct nvme_features features;
	struct nvme_create_cq create_cq;
	struct nvme_create_sq create_sq;
	struct nvme_delete_queue delete_queue;
	struct nvme_download_firmware dlfw;
	struct nvme_format_cmd format;
	struct nvme_dsm_cmd dsm;
	struct nvme_write_zeroes_cmd write_zeroes;
	struct nvme_zone_mgmt_send_cmd zms;
	struct nvme_zone_mgmt_recv_cmd zmr;
	struct nvme_abort_cmd abort;
	struct nvme_get_log_page_command get_log_page;
	struct nvmf_common_command fabrics;
	struct nvmf_connect_command connect;
	struct nvmf_property_set_command prop_set;
	struct nvmf_property_get_command prop_get;
	//struct nvmf_auth_common_command auth_common;
	//struct nvmf_auth_send_command auth_send;
	//struct nvmf_auth_receive_command auth_receive;
	struct nvme_dbbuf dbbuf;
	struct nvme_directive_cmd directive;

} nvme_sq_entry_t;

struct psf_ctx {
	u16             p : 1;
	u16             sc : 8;
	u16             sct : 3;
	u16             rsvd3 : 2;
	u16             m : 1;
	u16             dnr : 1;
};

typedef struct _nvme_cq_entry {
	u32                     cs;
	u32                     rsvd;
	u16                     sqhd;
	u16                     sqid;
	u16                     cid;
	union {
		u16                 psf;
		struct psf_ctx  a;
	}u;
} nvme_cq_entry_t;



struct nvme_smart_log {
	u8  critical_warning;
	u8  temperature[2];
	u8  avail_spare;
	u8  spare_thresh;
	u8  percent_used;
	u8  rsvd6[26];
	u8  data_units_read[16];
	u8  data_units_written[16];
	u8  host_reads[16];
	u8  host_writes[16];
	u8  ctrl_busy_time[16];
	u8  power_cycles[16];
	u8  power_on_hours[16];
	u8  unsafe_shutdowns[16];
	u8  media_errors[16];
	u8  num_err_log_entries[16];
	u32   warning_temp_time;
	u32   critical_comp_time;
	u16 temp_sensor[8];
	u32   thm_temp1_trans_count;
	u32   thm_temp2_trans_count;
	u32   thm_temp1_total_time;
	u32   thm_temp2_total_time;
	u8  rsvd232[280];
};